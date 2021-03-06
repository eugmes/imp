{-# LANGUAGE FlexibleContexts #-}

module IMP.Codegen.Error
  ( CodegenError(..)
  , throwLocatedError
  , locatedErrorPretty
  ) where

import IMP.AST
import IMP.SourceLoc
import Text.Megaparsec.Error -- TODO use custom class here
import Text.Printf
import Control.Monad.Except

data CodegenError = InternalError String
                  | TypeMismatch Type Type -- ^ Types don't match
                  | InvalidNumberOfArguments -- ^ Invalid number of arguments
                  | NonBooleanIfCondition -- ^ Boolean expression is expected as if statement condition
                  | NonBooleanWhileCondition -- ^ Boolean expression is expected as while statement condition
                  | NotAVariable ID -- ^ ... is not a variable
                  | AttemptToCallAVariable -- ^ Attempt to call a variable
                  | AttemptToCallAFunctionAsProcedure
                  | AttemptToCallAProcedureAsFunction
                  | InputError
                  | BreakOutsideOfLoop
                  | VoidReturnInFunction
                  | NonVoidReturnInProcedure
                  | UnaryOpTypeMismatch UnaryOp Type
                  | BinaryOpTypeMismatch BinaryOp Type
                  | AttemptToReadSubroutine
                  | GlobalRedefinition ID
                  | LocalRedefinition ID
                  | SymbolNotInScope ID
                  | MainIsAFunction
                  | MainHasArguments
                  | IntegerLiteralOutOfTypeRange Number
                  | AssignmentToConstant
                  | ConstantExpressionAsParameter Mode
                  deriving (Eq, Ord)

eShow :: CodegenError -> String
eShow (InternalError msg) = printf "Internal error: %s" msg
eShow (TypeMismatch leftType rightType) = printf "Types dont't match: '%s' vs '%s'." (show leftType) (show rightType)
eShow InvalidNumberOfArguments = "Invalid number of arguments."
eShow NonBooleanIfCondition = "Boolean expression is expected as if statement condition."
eShow NonBooleanWhileCondition = "Boolean expression is expected as while statement condition."
eShow (NotAVariable name) = printf "'%s' is not a variable." (getID name)
eShow AttemptToCallAVariable = "Attempt to call a variable."
eShow AttemptToCallAFunctionAsProcedure = "Attempt to call a function as procedure."
eShow AttemptToCallAProcedureAsFunction = "Attempt to call a procedure as function."
eShow InputError = "Attempt to input something other than integer."
eShow BreakOutsideOfLoop = "'break' outside of a loop."
eShow VoidReturnInFunction = "Function should return a value."
eShow NonVoidReturnInProcedure = "Procedure cannot return a value."
eShow (UnaryOpTypeMismatch unaryOp ty) = printf "Type mismatch for unary operator '%s': '%s'." (show unaryOp) (show ty)
eShow (BinaryOpTypeMismatch binaryOp ty) = printf "Type mismatch for binary operator '%s': '%s'." (show binaryOp) (show ty)
eShow AttemptToReadSubroutine = "Attempt to read value of subroutine."
eShow (GlobalRedefinition name) = printf "Attempt to redefine global symbol '%s'." (getID name)
eShow (LocalRedefinition name) = printf "Attempt to redefine local symbol '%s'." (getID name)
eShow (SymbolNotInScope name) = printf "Symbol not in scope: '%s'." (getID name)
eShow MainIsAFunction = "'main' should be a procedure."
eShow MainHasArguments = "'main' should be a procedure with no arguments."
eShow (IntegerLiteralOutOfTypeRange (Number n)) = printf "Integer literal is outside of allowed range: %d." n
eShow AssignmentToConstant = printf "Cannot assign value to constant."
eShow (ConstantExpressionAsParameter mode) = printf "Attempt to pass a constant expression as parameter with mode %s." (show mode) -- TODO: better wording

instance ShowErrorComponent CodegenError where
  showErrorComponent = eShow

throwLocatedError :: (WithLoc m, MonadError (Located e) m) => e -> m a
throwLocatedError e = do
  loc <- currentLoc
  throwError $ Located loc e

-- TODO remove this hack and create some custom class
locatedErrorPretty :: ShowErrorComponent a => Located a -> String
locatedErrorPretty e = showErrorComponent $ unLoc e
