{-# LANGUAGE OverloadedStrings #-}

module IMP.Codegen.Utils
  ( StandardCall(..)
  , SymbolType(..)
  , SymbolTable
  , stdCallName
  , stdCallType
  , stdCallArgs
  , stdCallAttrs
  , stdCallOp
  , typeToLLVM
  , integer
  , constTrue
  , constFalse
  , constZero
  , boolean
  , checkIntegerBounds
  ) where

import qualified IMP.AST as I
import qualified IMP.SymbolTable as Tab

import LLVM.AST
import qualified LLVM.AST.FunctionAttribute as FA
import LLVM.AST.Constant as C
import LLVM.AST.Type
import LLVM.AST.AddrSpace
import Data.Int

data SymbolType = SymbolVariable I.Type
                | SymbolProcedure [I.Type]
                | SymbolFunction I.Type [I.Type]
                deriving Show

data StandardCall = CallInputInteger
                  | CallOutputInteger
                  | CallOutputBoolean
                  | CallOutputString
                  | CallHalt
                  | CallNewline
                  | CallSAddWithOverflow
                  | CallSSubWithOverflow
                  | CallSMulWithOverflow
                  | CallDivideByZeroEx
                  | CallIntegerOverflowEx
                  deriving (Show, Ord, Bounded, Eq)

stdCallName :: StandardCall -> Name
stdCallName CallInputInteger = "_IMP_input_integer"
stdCallName CallOutputInteger = "_IMP_output_integer"
stdCallName CallOutputBoolean = "_IMP_output_boolean"
stdCallName CallOutputString = "_IMP_output_string"
stdCallName CallHalt = "_IMP_halt"
stdCallName CallNewline = "_IMP_newline"
-- NOTE this call names will need to change if integer size is changed
stdCallName CallSAddWithOverflow = "llvm.sadd.with.overflow.i32"
stdCallName CallSSubWithOverflow = "llvm.ssub.with.overflow.i32"
stdCallName CallSMulWithOverflow = "llvm.smul.with.overflow.i32"
stdCallName CallDivideByZeroEx = "_IMP_divide_by_zero_ex"
stdCallName CallIntegerOverflowEx = "_IMP_integer_overflow_ex"

stdCallType :: StandardCall -> Type
stdCallType CallInputInteger = integer
stdCallType CallSAddWithOverflow = integerAndBoolean
stdCallType CallSSubWithOverflow = integerAndBoolean
stdCallType CallSMulWithOverflow = integerAndBoolean
stdCallType _ = VoidType

stdCallArgs :: StandardCall -> [Type]
stdCallArgs CallOutputInteger = [integer]
stdCallArgs CallOutputBoolean = [boolean]
stdCallArgs CallOutputString = [stringType]
stdCallArgs CallSAddWithOverflow = [integer, integer]
stdCallArgs CallSSubWithOverflow = [integer, integer]
stdCallArgs CallSMulWithOverflow = [integer, integer]
stdCallArgs _ = []

stdCallAttrs :: StandardCall -> [FA.FunctionAttribute]
stdCallAttrs CallHalt = [FA.NoReturn]
stdCallAttrs CallDivideByZeroEx = [FA.NoReturn]
stdCallAttrs CallIntegerOverflowEx = [FA.NoReturn]
stdCallAttrs _ = []

stdCallOp :: StandardCall -> Operand
stdCallOp c = ConstantOperand $ GlobalReference ty $ stdCallName c
 where
  retty = stdCallType c
  ty = ptr $ FunctionType retty (stdCallArgs c) False

type SymbolTableEntry = (SymbolType, Operand)

type SymbolTable = Tab.SymbolTable I.ID SymbolTableEntry

integer, boolean, stringType, integerAndBoolean :: Type
integer = i32
boolean = i1
stringType = PointerType i8 $ AddrSpace 0
integerAndBoolean = StructureType False [integer, boolean]

checkIntegerBounds :: Integer -> Bool
checkIntegerBounds n = n >= minInteger  && n <= maxInteger
 where
  minInteger = fromIntegral (minBound :: Int32)
  maxInteger = fromIntegral (maxBound :: Int32)

constFalse, constTrue :: Operand
constFalse = ConstantOperand $ C.Int 1 0
constTrue = ConstantOperand $ C.Int 1 1

constZero :: Type -> Operand
constZero ty = ConstantOperand $ C.Int (typeBits ty) 0

typeToLLVM :: I.Type -> Type
typeToLLVM I.IntegerType = integer
typeToLLVM I.BooleanType = boolean
