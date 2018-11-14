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
  , mkName
  ) where

import qualified IMP.AST as I
import qualified IMP.SymbolTable as Tab

import LLVM.AST hiding (mkName)
import qualified LLVM.AST.FunctionAttribute as FA
import LLVM.AST.Constant as C
import LLVM.AST.Type
import LLVM.AST.AddrSpace
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString.Short (toShort)

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
                  | CallConstraintErrorEx
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
stdCallName CallConstraintErrorEx = "_IMP_constraint_error_ex"

stdCallType :: StandardCall -> Type
stdCallType CallInputInteger = integer
stdCallType CallSAddWithOverflow = integerAndBoolean
stdCallType CallSSubWithOverflow = integerAndBoolean
stdCallType CallSMulWithOverflow = integerAndBoolean
stdCallType _ = VoidType

stdCallArgs :: StandardCall -> [(Type, Name)]
stdCallArgs CallOutputInteger = [(integer, "val")]
stdCallArgs CallOutputBoolean = [(boolean, "val")]
stdCallArgs CallOutputString = [(stringType, "s")]
stdCallArgs CallSAddWithOverflow = [(integer, "a"), (integer, "b")]
stdCallArgs CallSSubWithOverflow = [(integer, "a"), (integer, "b")]
stdCallArgs CallSMulWithOverflow = [(integer, "a"), (integer, "b")]
stdCallArgs CallConstraintErrorEx = [(stringType, "file_name"), (integer, "line_no")]
stdCallArgs _ = []

stdCallAttrs :: StandardCall -> [FA.FunctionAttribute]
stdCallAttrs CallHalt = [FA.NoReturn]
stdCallAttrs CallConstraintErrorEx = [FA.NoReturn]
stdCallAttrs _ = []

stdCallOp :: StandardCall -> Operand
stdCallOp c = ConstantOperand $ GlobalReference ty $ stdCallName c
 where
  retty = stdCallType c
  ty = ptr $ FunctionType retty (fst <$> stdCallArgs c) False

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
typeToLLVM I.StringType = stringType

mkName :: T.Text -> Name
mkName = Name . toShort . TE.encodeUtf8
