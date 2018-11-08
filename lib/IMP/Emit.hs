{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module IMP.Emit ( CodegenOptions(..)
                , compileProgram
                ) where

import qualified IMP.AST as I
import IMP.AST (getID)
import IMP.Codegen.GlobalCodegen
import IMP.Codegen.SubCodegen
import IMP.Codegen.Utils
import IMP.Codegen.Error
import IMP.SourceLoc
import qualified LLVM.AST as AST
import LLVM.AST hiding (type', mkName)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import Control.Monad
import qualified Data.Text as T

compileProgram :: CodegenOptions -> I.Program -> Either (Located CodegenError) AST.Module
compileProgram opts = execGlobalCodegen opts . codegenProgram

codegenProgram :: I.Program -> GlobalCodegen ()
codegenProgram (I.Program vars subs) = do
  mapM_ (withLoc codegenVars) vars
  mapM_ (withLoc codegenSubDecl) subs
  mapM_ (withLoc codegenSub) subs

codegenVars :: I.VarDec -> GlobalCodegen ()
codegenVars (I.VarDec names t) = mapM_ (withLoc $ defineVar $ unLoc t) names

toSig :: [Located I.ParamList] -> [(I.Type, Located I.ID)]
toSig = concatMap (toSig' . unLoc)
 where
  toSig' (I.ParamList ids ty) = map (toSig'' $ unLoc ty) ids
  toSig'' t name = (t, name)

paramTypes :: [Located I.ParamList] -> [I.Type]
paramTypes = fmap fst . toSig

codegenSubDecl :: I.Subroutine -> GlobalCodegen ()
codegenSubDecl (I.Procedure name params _ _) =
  withLoc (`declareProc` ts) name
 where
  ts = paramTypes params

codegenSubDecl (I.Function name params retty _ _) =
  withLoc (\n -> declareFun n (unLoc retty) ts) name
 where
  ts = paramTypes params

codegenSub' :: Located I.ID -> Maybe I.Type -> [Located I.ParamList] -> [Located I.VarDec] -> I.Statements -> GlobalCodegen ()
codegenSub' name retty params vars body = do
  blocks <- execSubCodegen cg
  defineSub (unLoc name) retty args blocks
 where
  args = toSig params
  cg = mdo
    _ <- block "entry"
    retval <- forM retty $ \t -> do
      op <- alloca ".return" $ typeToLLVM t
      return (t, op)

    setExitBlock retval exit

    forM_ args $ \(ty, a) -> do
      let t = typeToLLVM ty
      var <- alloca (getID (unLoc a) <> ".addr") t
      store var $ LocalReference t $ mkName $ getID $ unLoc a
      withLoc (\n -> defineLocalVar n ty var) a
    codegenLocals vars
    mapM_ (withLoc codegenStatement) body
    br exit

    exit <- block "exit"
    if unLoc name == I.ID "main"
      then do
        apiProcCall CallHalt []
        unreachable
      else
        ret =<< mapM (\(ty, op) -> load (typeToLLVM ty) op) retval

codegenSub :: I.Subroutine -> GlobalCodegen ()
codegenSub (I.Procedure name params vars body) = do
  when ((unLoc name == I.ID "main") && not (null params)) $
    throwLocatedError MainHasArguments
  codegenSub' name Nothing params vars body

codegenSub (I.Function name params retty vars body) = do
  when (unLoc name == I.ID "main") $ throwLocatedError MainIsAFunction
  codegenSub' name (Just $ unLoc retty) params vars body

codegenLocals :: [Located I.VarDec] -> SubCodegen ()
codegenLocals = mapM_ $ withLoc codegenLocals'

codegenLocals' :: I.VarDec -> SubCodegen ()
codegenLocals' (I.VarDec names ty) = mapM_ (withLoc $ \n -> codegenLocal n $ unLoc ty) names

codegenLocal :: I.ID -> I.Type -> SubCodegen ()
codegenLocal name ty = do
  var <- alloca (getID name) $ typeToLLVM ty
  defineLocalVar name ty var

typeCheck :: I.Type -> I.Type -> SubCodegen ()
typeCheck lt rt =
  when (lt /= rt) $ throwLocatedError $ TypeMismatch lt rt

-- | Generate code and typecheck expression
--
-- Errors are emitted on expression location.
typeCheckExpression :: I.Type -> Located I.Expression -> SubCodegen Operand
typeCheckExpression t = withLoc (codegenExpression >=> check)
 where
  check (t', op) = typeCheck t t' >> return op

maybeGenBlock :: T.Text -> Name -> I.Statements -> SubCodegen Name
maybeGenBlock _ contName [] = return contName

maybeGenBlock newTemlate contName stmts = mdo
  e <- entry
  newName <- block newTemlate
  mapM_ (withLoc codegenStatement) stmts
  br contName

  setBlock e
  return newName

-- | Uncoditionally transfer execution to another block
--
-- Sets a new active block. Operations emitted into this
-- block will be discarded
goto :: Name -> SubCodegen ()
goto bname = do
  br bname
  _ <- block "discard"
  return ()

-- | Generate and typecheck subroutine arguments
--
-- Errors are reported at location of argument that have caused that error.
codegenArgs :: [I.Type] -> [Located I.Expression] -> SubCodegen [Operand]
codegenArgs args exps = do
  when (length args /= length exps) $
    throwLocatedError InvalidNumberOfArguments
  zipWithM typeCheckExpression args exps

codegenStatement :: I.Statement -> SubCodegen ()

codegenStatement (I.IfStatement condPart elseStmts) = mdo
  mapM_ (emitCondPart exitB) condPart
  mapM_ (withLoc codegenStatement) elseStmts
  br exitB

  exitB <- block "if.exit"
  return ()
 where
  check = checkBoolean NonBooleanIfCondition

  emitCondPart exitB (cond, stmts) = mdo
    op <- withLoc (codegenExpression >=> check) cond

    ifB <- maybeGenBlock "then" exitB stmts
    cbr op ifB elseB

    elseB <- block "else"
    return ()

codegenStatement (I.WhileStatement cond body) = mdo
  br whileCondB

  whileCondB <- block "while.cond"
  op <- withLoc (codegenExpression >=> check) cond
  whileLoopB <- withLoopExit whileExitB $ maybeGenBlock "while.loop" whileCondB body
  cbr op whileLoopB whileExitB

  whileExitB <- block "while.exit"
  return ()
 where
   check = checkBoolean NonBooleanWhileCondition

codegenStatement (I.AssignStatement name exp) = do
  (varSymType, varPtr) <- withLoc getVar name
  varType <- case varSymType of
    SymbolVariable ty -> return ty
    _ -> throwLocatedError $ NotAVariable $ unLoc name
  (expType, op) <- withLoc codegenExpression exp
  typeCheck varType expType
  store varPtr op

codegenStatement (I.CallStatement name exps) = do
  (ty, proc) <- withLoc getVar name
  case ty of
    SymbolVariable _ -> throwLocatedError AttemptToCallAVariable
    SymbolFunction _ _ -> throwLocatedError AttemptToCallAFunctionAsProcedure
    SymbolProcedure args -> do
      argOps <- codegenArgs args exps
      callProc proc argOps []

codegenStatement (I.InputStatement name) = do
  (ty, ptr) <- withLoc getVar name
  case ty of
    SymbolVariable t -> do
      op <- case t of
        I.IntegerType -> apiFunCall CallInputInteger []
        _ -> throwLocatedError InputError
      store ptr op
    _ -> throwLocatedError InputError

codegenStatement (I.OutputStatement exp) = do
  (ty, op) <- withLoc codegenExpression exp
  let api = case ty of
            I.IntegerType -> CallOutputInteger
            I.BooleanType -> CallOutputBoolean
            I.StringType -> CallOutputString
  apiProcCall api [op]

codegenStatement I.NullStatement = return ()

codegenStatement I.BreakStatement =
  getLoopExitBlock >>= maybe (throwLocatedError BreakOutsideOfLoop) goto

codegenStatement I.ReturnStatement =
  exit >>= \case
    (_, Just _) -> throwLocatedError VoidReturnInFunction
    (bname, Nothing) -> goto bname

codegenStatement (I.ReturnValStatement exp) =
  exit >>= \case
    (bname, Just (retty, ptr)) -> do
      op <- typeCheckExpression retty exp
      store ptr op
      goto bname
    (_, Nothing) -> throwLocatedError NonVoidReturnInProcedure

codegenStatement I.HaltStatement = apiProcCall CallHalt []
codegenStatement I.NewlineStatement = apiProcCall CallNewline []

codegenExpression :: I.Expression -> SubCodegen (I.Type, Operand)
codegenExpression (I.UnOpExp unaryOp expr) = do
  (ty, fOp) <- withLoc codegenExpression expr
  case (unaryOp, ty) of
    (I.OpNot, I.BooleanType) -> do
      op <- notInstr fOp
      return (ty, op)
    (I.OpNeg, I.IntegerType) ->
      genArithCall ty CallSSubWithOverflow (constZero $ typeToLLVM ty) fOp
    _ -> throwLocatedError $ UnaryOpTypeMismatch unaryOp ty

codegenExpression (I.BinOpExp leftExp binaryOp rightExp) = do
  (leftType, leftOp) <- withLoc codegenExpression leftExp
  (rightType, rightOp) <- withLoc codegenExpression rightExp
  typeCheck leftType rightType

  let fn = case (binaryOp, leftType) of
           (I.OpEQ, I.BooleanType) -> icmp IP.EQ
           (I.OpEQ, I.IntegerType) -> icmp IP.EQ
           (I.OpLT, I.IntegerType) -> icmp IP.SLT
           (I.OpLE, I.IntegerType) -> icmp IP.SLE
           (I.OpGT, I.IntegerType) -> icmp IP.SGT
           (I.OpGE, I.IntegerType) -> icmp IP.SLE
           (I.OpNE, I.BooleanType) -> icmp IP.NE
           (I.OpNE, I.IntegerType) -> icmp IP.NE
           (I.OpAdd, I.IntegerType) -> genArithCall leftType CallSAddWithOverflow
           (I.OpSub, I.IntegerType) -> genArithCall leftType CallSSubWithOverflow
           (I.OpOr, I.BooleanType) -> or
           (I.OpMul, I.IntegerType) -> genArithCall leftType CallSMulWithOverflow
           (I.OpDiv, I.IntegerType) -> genDivOp leftType sdiv
           (I.OpMod, I.IntegerType) -> genDivOp leftType srem
           (I.OpAnd, I.BooleanType) -> and
           _ -> \_ _ -> throwLocatedError $ BinaryOpTypeMismatch binaryOp leftType

  fn leftOp rightOp
 where
  wrap ty f op0 op1 = do
    op <- instr (typeToLLVM ty) $ f op0 op1 []
    return (ty, op)

  icmp pred = wrap I.BooleanType (AST.ICmp pred)
  or = wrap I.BooleanType AST.Or
  sdiv = wrap I.IntegerType $ AST.SDiv False
  srem = wrap I.IntegerType AST.SRem
  and = wrap I.BooleanType AST.And

codegenExpression (I.NumberExpression c@(I.Number num)) =
  if checkIntegerBounds num
    then return (I.IntegerType, op)
    else throwLocatedError $ IntegerLiteralOutOfTypeRange c
 where
  op = ConstantOperand $ C.Int (typeBits integer) num

codegenExpression (I.BoolExpression val) =
  return (I.BooleanType, op)
 where
  op = if val then constTrue else constFalse

codegenExpression (I.IdExpression name) = do
  (ty, ptr) <- withLoc getVar name
  case ty of
    SymbolVariable t -> do
      op <- load (typeToLLVM t) ptr
      return (t, op)
    _ -> throwLocatedError AttemptToReadSubroutine

codegenExpression (I.CallExpression name exps) = do
  (ty, fun) <- withLoc getVar name
  case ty of
    SymbolVariable _ -> throwLocatedError AttemptToCallAVariable
    SymbolProcedure _ -> throwLocatedError AttemptToCallAProcedureAsFunction
    SymbolFunction rtype args -> do
      argOps <- codegenArgs args exps
      op <- callFun (typeToLLVM rtype) fun argOps []
      return (rtype, op)

codegenExpression (I.StringLiteralExpression str) = do
  op <- withLoc emitString str
  return (I.StringType, op)

-- | Generate a division operation with division by zero check.
--
-- TODO Make check optional
-- TODO Adjust weights of branches
genDivOp :: I.Type -> (Operand -> Operand -> SubCodegen (I.Type, Operand))
            -> Operand -> Operand -> SubCodegen (I.Type, Operand)
genDivOp ty fn leftOp rightOp = mdo
  condOp <- instr boolean $ AST.ICmp IP.EQ rightOp (constZero $ typeToLLVM ty) []
  cbr condOp exB divB

  exB <- block "div.ex"
  apiProcCall CallDivideByZeroEx []
  unreachable

  divB <- block "div"
  fn leftOp rightOp

-- | Generate integer arithmetic operations with overflow check.
--
-- TODO Make check optional
-- TODO Adjust weights of branches
genArithCall :: I.Type -> StandardCall -> Operand -> Operand -> SubCodegen (I.Type, Operand)
genArithCall ty c leftOp rightOp = mdo
  op <- apiFunCall c [leftOp, rightOp]
  obit <- instr boolean $ ExtractValue op [1] []
  cbr obit exB arithB

  exB <- block "arith.ex"
  apiProcCall CallIntegerOverflowEx []
  unreachable

  arithB <- block "arith"
  res <- instr (typeToLLVM ty) $ ExtractValue op [0] []
  return (ty, res)

checkBoolean :: CodegenError -> (I.Type, Operand) -> SubCodegen Operand
checkBoolean _ (I.BooleanType, op) = return op
checkBoolean err _ = throwLocatedError err
