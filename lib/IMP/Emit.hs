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
import IMP.Types
import qualified LLVM.AST as AST
import LLVM.AST hiding (type', mkName)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import Control.Monad
import qualified Data.Text as T
import Text.Megaparsec.Pos (unPos) -- FIXME: Don't use megeparsec here
import Data.Maybe

data ExpressionValue = ValueReference I.Type Operand
                     -- ^ Reference to a non-constant variable
                     | Value I.Type Operand
                     -- ^ Raw value, unusable for out or in out parameters

mkValue :: (I.Type, Operand) -> SubCodegen ExpressionValue
mkValue (ty, op) = pure $ Value ty op

dereference :: ExpressionValue -> SubCodegen (I.Type, Operand)
dereference (ValueReference ty ptr) = do
  op <- load (typeToLLVM ty) ptr
  pure (ty, op)

dereference (Value ty op) = pure (ty, op)

compileProgram :: CodegenOptions -> I.Program -> Either (Located CodegenError) AST.Module
compileProgram opts = execGlobalCodegen opts . codegenProgram

codegenProgram :: I.Program -> GlobalCodegen ()
codegenProgram (I.Program vars subs) = do
  mapM_ (withLoc codegenVars) vars
  mapM_ (withLoc codegenSubDecl) subs
  mapM_ (withLoc codegenSub) subs

-- | Emit definitions of global variables
codegenVars :: I.VarDec -> GlobalCodegen ()
codegenVars (I.VarDec names t) = mapM_ (withLoc $ defineVar (unLoc t) NotConstant) names

toSig :: [Located I.ParamList] -> [(I.Type, I.Mode, Located I.ID)]
toSig = concatMap (toSig' . unLoc)
 where
  toSig' (I.ParamList ids mode ty) = map (toSig'' (unLoc ty) mode) ids
  toSig'' t mode name = (t, mode, name)

paramTypes :: [Located I.ParamList] -> [Argument]
paramTypes = fmap (\(ty, mode, _) -> (ty, mode)) . toSig

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

    argLocs <- forM args $ \(ty, mode, a) -> do
      let t = typeToLLVM ty
          arg = LocalReference t $ mkName $ getID $ unLoc a
      varPtr <- alloca (getID (unLoc a) <> ".addr") t
      case mode of
        I.ModeIn -> do
          store varPtr arg
          withLoc (\n -> defineLocalVar n ty IsConstant varPtr) a
          pure Nothing
        I.ModeOut -> do
          -- Mode out variables are not initialized
          withLoc (\n -> defineLocalVar n ty NotConstant varPtr) a
          pure $ Just (ty, varPtr)
        I.ModeInOut -> do
          store varPtr arg
          withLoc (\n -> defineLocalVar n ty NotConstant varPtr) a
          pure $ Just (ty, varPtr)
    codegenLocals vars
    mapM_ (withLoc codegenStatement) body
    case retty of
      Just _ -> do
        raiseProgramError
        unreachable
      Nothing ->
        br exit

    exit <- block "exit"
    if unLoc name == I.ID "main"
      then do
        apiProcCall CallHalt []
        unreachable
      else
        case catMaybes (retval : argLocs) of
          [] -> ret Nothing
          [(ty, ptr)] -> do
            op <- load (typeToLLVM ty) ptr
            ret $ Just op
          refs -> do
            let actualReturnType = StructureType False $ map (typeToLLVM . fst) refs
                storeResult aggr ((ty, op), idx) = do
                  tmp <- load (typeToLLVM ty) op
                  instr actualReturnType $ InsertValue aggr tmp [idx] []

            retAggr <- foldM storeResult (ConstantOperand $ C.Undef actualReturnType) (zip refs [0..])
            ret $ Just retAggr

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

-- FIXME Currently all local variables are not constant
codegenLocal :: I.ID -> I.Type -> SubCodegen ()
codegenLocal name ty = do
  var <- alloca (getID name) $ typeToLLVM ty
  defineLocalVar name ty NotConstant var

typeCheck :: I.Type -> I.Type -> SubCodegen ()
typeCheck lt rt =
  when (lt /= rt) $ throwLocatedError $ TypeMismatch lt rt

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

-- | Returns operands for input arguments and types and references for output arguments
codegenArgs :: [Argument] -> [Located I.Expression] -> SubCodegen ([Operand], [(I.Type, Operand)])
codegenArgs args exps = do
  when (length args /= length exps) $ throwLocatedError InvalidNumberOfArguments
  r <- zipWithM typeCheckArg args exps
  pure (mapMaybe fst r, mapMaybe snd r)

typeCheckArg :: Argument -> Located I.Expression -> SubCodegen (Maybe Operand, Maybe (I.Type, Operand))
typeCheckArg (argTy, mode) =
  withLoc (codegenExpression >=> check mode)
 where
  check I.ModeIn val = do
    (expTy, op) <- dereference val
    typeCheck argTy expTy
    pure (Just op, Nothing)

  check I.ModeOut val =
    case val of
      (ValueReference expTy ptr) -> do
        typeCheck argTy expTy
        pure (Nothing, Just (expTy, ptr))
      _ -> throwLocatedError $ ConstantExpressionAsParameter mode

  check I.ModeInOut val =
    case val of
      (ValueReference expTy ptr) -> do
        typeCheck argTy expTy
        (_, op) <- dereference val
        pure (Just op, Just (expTy, ptr))
      _ -> throwLocatedError $ ConstantExpressionAsParameter mode

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
  case varSymType of
    SymbolVariable varType NotConstant -> do
      (expType, op) <- dereference =<< withLoc codegenExpression exp
      typeCheck varType expType
      store varPtr op
    SymbolVariable _ IsConstant -> throwLocatedError AssignmentToConstant
    _ -> throwLocatedError $ NotAVariable $ unLoc name

codegenStatement (I.CallStatement name exps) = do
  (ty, proc) <- withLoc getVar name
  case ty of
    SymbolVariable _ _ -> throwLocatedError AttemptToCallAVariable
    SymbolFunction _ _ -> throwLocatedError AttemptToCallAFunctionAsProcedure
    SymbolProcedure args -> do
      (inOps, outRefs) <- codegenArgs args exps
      ops <- callFunc proc inOps [] (map (typeToLLVM . fst) outRefs)
      forM_ (zip outRefs ops) $ \((_, ptr), op) ->
        store ptr op

codegenStatement (I.InputStatement name) = do
  (ty, ptr) <- withLoc getVar name
  case ty of
    SymbolVariable t NotConstant -> do
      op <- case t of
        I.IntegerType -> apiFunCall CallInputInteger []
        _ -> throwLocatedError InputError
      store ptr op
    SymbolVariable _ IsConstant -> throwLocatedError AssignmentToConstant
    _ -> throwLocatedError InputError

codegenStatement (I.OutputStatement exp) = do
  (ty, op) <- dereference =<< withLoc codegenExpression exp
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
      op <- withLoc (codegenExpression >=> check retty) exp
      store ptr op
      goto bname
    (_, Nothing) -> throwLocatedError NonVoidReturnInProcedure
 where
  check t val = do
    (t', op) <- dereference val
    typeCheck t t'
    return op

codegenStatement I.HaltStatement = apiProcCall CallHalt []
codegenStatement I.NewlineStatement = apiProcCall CallNewline []

codegenExpression :: I.Expression -> SubCodegen ExpressionValue
codegenExpression (I.UnOpExp unaryOp expr) = do
  (ty, fOp) <- dereference =<< withLoc codegenExpression expr
  case (unaryOp, ty) of
    (I.OpNot, I.BooleanType) -> do
      op <- notInstr fOp
      mkValue (ty, op)
    (I.OpNeg, I.IntegerType) ->
      genArithCall ty CallSSubWithOverflow (constZero $ typeToLLVM ty) fOp
    _ -> throwLocatedError $ UnaryOpTypeMismatch unaryOp ty

codegenExpression (I.BinOpExp leftExp binaryOp rightExp) = do
  (leftType, leftOp) <- dereference =<< withLoc codegenExpression leftExp
  (rightType, rightOp) <- dereference =<< withLoc codegenExpression rightExp
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
    mkValue (ty, op)

  icmp pred = wrap I.BooleanType (AST.ICmp pred)
  or = wrap I.BooleanType AST.Or
  sdiv = wrap I.IntegerType $ AST.SDiv False
  srem = wrap I.IntegerType AST.SRem
  and = wrap I.BooleanType AST.And

codegenExpression (I.NumberExpression c@(I.Number num)) =
  if checkIntegerBounds num
    then mkValue (I.IntegerType, op)
    else throwLocatedError $ IntegerLiteralOutOfTypeRange c
 where
  op = ConstantOperand $ C.Int (typeBits integer) num

codegenExpression (I.BoolExpression val) =
  mkValue (I.BooleanType, op)
 where
  op = if val then constTrue else constFalse

codegenExpression (I.IdExpression name) = do
  (ty, ptr) <- withLoc getVar name
  case ty of
    SymbolVariable t NotConstant ->
      pure $ ValueReference t ptr
    SymbolVariable t IsConstant -> do
      op <- load (typeToLLVM t) ptr
      mkValue (t, op)
    _ -> throwLocatedError AttemptToReadSubroutine

codegenExpression (I.CallExpression name exps) = do
  (ty, fun) <- withLoc getVar name
  case ty of
    SymbolVariable _ _ -> throwLocatedError AttemptToCallAVariable
    SymbolProcedure _ -> throwLocatedError AttemptToCallAProcedureAsFunction
    SymbolFunction retty args -> do
      (inOps, outRefs) <- codegenArgs args exps
      callFunc fun inOps [] (typeToLLVM retty : map (typeToLLVM . fst) outRefs) >>= \case
        (op : ops) -> do
          forM_ (zip outRefs ops) $ \((_, ptr), op) ->
            store ptr op
          mkValue (retty, op)
        _ -> throwLocatedError $ InternalError "Function missing return value"

codegenExpression (I.StringLiteralExpression str) = do
  op <- withLoc emitString str
  mkValue (I.StringType, op)

raiseError :: StandardCall -> SubCodegen ()
raiseError call = do
  loc <- currentLoc
  fileNameOp <- emitString (T.pack $ sourceName loc)
  let lineNumOp = ConstantOperand $ C.Int (typeBits integer) $ fromIntegral $ unPos $ sourceLine loc
  apiProcCall call [fileNameOp, lineNumOp]

raiseConstraintError :: SubCodegen ()
raiseConstraintError = raiseError CallConstraintErrorEx

raiseProgramError :: SubCodegen ()
raiseProgramError = raiseError CallProgramErrorEx

-- | Generate a division operation with division by zero check.
--
-- TODO Make check optional
-- TODO Adjust weights of branches
genDivOp :: I.Type -> (Operand -> Operand -> SubCodegen ExpressionValue)
            -> Operand -> Operand -> SubCodegen ExpressionValue
genDivOp ty fn leftOp rightOp = mdo
  condOp <- instr boolean $ AST.ICmp IP.EQ rightOp (constZero $ typeToLLVM ty) []
  cbr condOp exB divB

  exB <- block "div.ex"
  raiseConstraintError
  unreachable

  divB <- block "div"
  fn leftOp rightOp

-- | Generate integer arithmetic operations with overflow check.
--
-- TODO Make check optional
-- TODO Adjust weights of branches
genArithCall :: I.Type -> StandardCall -> Operand -> Operand -> SubCodegen ExpressionValue
genArithCall ty c leftOp rightOp = mdo
  op <- apiFunCall c [leftOp, rightOp]
  obit <- instr boolean $ ExtractValue op [1] []
  cbr obit exB arithB

  exB <- block "arith.ex"
  raiseConstraintError
  unreachable

  arithB <- block "arith"
  res <- instr (typeToLLVM ty) $ ExtractValue op [0] []
  mkValue (ty, res)

checkBoolean :: CodegenError -> ExpressionValue -> SubCodegen Operand
checkBoolean err val = do
  (ty, op) <- dereference val
  case ty of
    I.BooleanType -> pure op
    _ -> throwLocatedError err
