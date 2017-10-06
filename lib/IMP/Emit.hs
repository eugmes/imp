{-# LANGUAGE OverloadedStrings #-}

module IMP.Emit (codegenProgram) where

import qualified IMP.AST as I
import IMP.AST (getID)
import IMP.Codegen
import IMP.Codegen.Error
import IMP.SourceLoc
import qualified LLVM.AST as AST
import LLVM.AST hiding (type')
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import Control.Monad.State

codegenProgram :: I.Program -> GlobalCodegen ()
codegenProgram (I.Program vars subs) = do
    mapM_ (withLoc codegenVars) vars
    mapM_ (withLoc codegenSubDecl) subs
    mapM_ (withLoc codegenSub) subs
    finalizeGlobalCodegen

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

codegenSub' :: Located I.ID -> Maybe I.Type -> [Located I.ParamList] -> [Located I.VarDec] -> [Located I.Statement] -> GlobalCodegen ()
codegenSub' name retty params vars body = do
    blocks <- execCodegen cg
    defineSub (unLoc name) retty args blocks
  where
    args = toSig params
    cg = do
        entry <- addBlock entryBlockName
        exit <- addBlock exitBlockName
        setBlock entry
        retval <- case retty of
                      Nothing -> return Nothing
                      Just t -> do
                        op <- alloca ".return" $ typeToLLVM t
                        return $ Just (t, op)

        setExitBlock retval exit

        forM_ args $ \(ty, a) -> do
            let t = typeToLLVM ty
            var <- alloca (getID (unLoc a) ++ ".addr") t
            store var $ local t $ mkName $ getID $ unLoc a
            withLoc (\n -> defineLocalVar n ty var) a
        codegenLocals vars
        mapM_ (withLoc codegenStatement) body
        br exit

        setBlock exit
        case retval of
            Nothing -> ret Nothing
            Just (ty, op) -> load (typeToLLVM ty) op >>= ret . Just

codegenSub :: I.Subroutine -> GlobalCodegen ()
codegenSub (I.Procedure name params vars body) = do
    when ((unLoc name == I.ID "main") && not (null params)) $
        throwLocatedError MainHasArguments
    codegenSub' name Nothing params vars body

codegenSub (I.Function name params retty vars body) = do
    when (unLoc name == I.ID "main") $ throwLocatedError MainIsAFunction
    codegenSub' name (Just $ unLoc retty) params vars body

codegenLocals :: [Located I.VarDec] -> Codegen ()
codegenLocals = mapM_ $ withLoc codegenLocals'

codegenLocals' :: I.VarDec -> Codegen ()
codegenLocals' (I.VarDec names ty) = mapM_ (withLoc $ \n -> codegenLocal n $ unLoc ty) names

codegenLocal :: I.ID -> I.Type -> Codegen ()
codegenLocal name ty = do
    var <- alloca (getID name) $ typeToLLVM ty
    defineLocalVar name ty var

typeCheck :: I.Type -> I.Type -> Codegen ()
typeCheck lt rt =
    when (lt /= rt) $ throwLocatedError $ TypeMismatch lt rt

maybeGenBlock :: String -> Name -> [Located I.Statement] -> Codegen Name
maybeGenBlock _ contName [] = return contName

-- TODO add block stack
maybeGenBlock newTemlate contName stmts = do
    e <- entry
    newName <- addBlock newTemlate
    setBlock newName
    mapM_ (withLoc codegenStatement) stmts
    br contName

    setBlock e
    return newName

-- | Uncoditionally transfer execution to another block
--
-- Sets a new active block. Operations emitted into this
-- block will be discarded
gotoBlock :: Name -> Codegen ()
gotoBlock bname = do
    br bname
    cont <- addBlock "discard"
    setBlock cont

codegenFunCall :: Type -> Operand -> [I.Type] -> [Located I.Expression] -> Codegen Operand
codegenFunCall retty name args exps = do
    when (length args /= length exps) $
        throwLocatedError InvalidNumberOfArguments
    vals <- mapM (withLoc codegenExpression) exps
    forM_ (zip args (map fst vals)) $ uncurry typeCheck
    callFun retty name (map snd vals) []

codegenProcCall :: Operand -> [I.Type] -> [Located I.Expression] -> Codegen ()
codegenProcCall name args exps = do
    when (length args /= length exps) $
        throwLocatedError InvalidNumberOfArguments
    vals <- mapM (withLoc codegenExpression) exps
    forM_ (zip args (map fst vals)) $ uncurry typeCheck
    callProc name (map snd vals) []

codegenStatement :: I.Statement -> Codegen ()

codegenStatement (I.IfStatement cond ifTrue ifFalse) = do
    op <- withLoc (codegenExpression >=> check) cond
    ifExitB <- addBlock "if.exit"
    ifTrueB <- maybeGenBlock "if.true" ifExitB ifTrue
    ifFalseB <- maybeGenBlock "if.false" ifExitB ifFalse

    cbr op ifTrueB ifFalseB

    setBlock ifExitB
  where
    check = checkBoolean NonBooleanIfCondition

codegenStatement (I.WhileStatement cond body) = do
    whileCondB <- addBlock "while.cond"
    whileExitB <- addBlock "while.exit"
    br whileCondB

    setBlock whileCondB

    op <- withLoc (codegenExpression >=> check) cond
    whileLoopB <- withLoopExit whileExitB $ maybeGenBlock "while.loop" whileCondB body
    cbr op whileLoopB whileExitB

    setBlock whileExitB
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
        SymbolProcedure args -> codegenProcCall proc args exps

codegenStatement (I.InputStatement name) = do
    (ty, ptr) <- withLoc getVar name
    case ty of
        SymbolVariable t -> do
            op <- case t of
                I.IntegerType -> apiFunCall CallInputInteger []
                _ -> throwLocatedError InputError
            store ptr op
        _ -> throwLocatedError InputError

codegenStatement (I.OutputStatement (I.Exp exp)) = do
    (ty, op) <- withLoc codegenExpression exp
    let api = case ty of
              I.IntegerType -> CallOutputInteger
              I.BooleanType -> CallOutputBoolean
    apiProcCall api [op]

codegenStatement (I.OutputStatement (I.Str str)) = do
    opPtr <- newString $ unLoc str
    apiProcCall CallOutputString [opPtr]

codegenStatement I.NullStatement = return ()

codegenStatement I.BreakStatement = do
    loopEx <- gets loopExitBlock
    case loopEx of
        Nothing -> throwLocatedError BreakOutsideOfLoop
        Just bname -> gotoBlock bname

codegenStatement I.ReturnStatement = do
    ex <- exit
    case ex of
        (_, Just _) -> throwLocatedError VoidReturnInFunction
        (bname, Nothing) -> gotoBlock bname

codegenStatement (I.ReturnValStatement exp) = do
    ex <- exit
    case ex of
        (bname, Just (retty, ptr)) -> do
            (ty, op) <- withLoc codegenExpression exp
            typeCheck retty ty
            store ptr op
            gotoBlock bname
        (_, Nothing) -> throwLocatedError NonVoidReturnInProcedure

codegenStatement I.HaltStatement = apiProcCall CallHalt []
codegenStatement I.NewlineStatement = apiProcCall CallNewline []

codegenExpression :: I.Expression -> Codegen (I.Type, Operand)
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
               (I.OpEQ, _) -> icmp IP.EQ
               (I.OpLT, I.IntegerType) -> icmp IP.SLT
               (I.OpLE, I.IntegerType) -> icmp IP.SLE
               (I.OpGT, I.IntegerType) -> icmp IP.SGT
               (I.OpGE, I.IntegerType) -> icmp IP.SLE
               (I.OpNE, _) -> icmp IP.NE
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

codegenExpression (I.NumberExpression (I.Number num)) =
    return (I.IntegerType, op)
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
            op <- codegenFunCall (typeToLLVM rtype) fun args exps
            return (rtype, op)

-- | Generate a division operation with division by zero check.
--
-- TODO Make check optional
-- TODO Adjust weights of branches
genDivOp :: I.Type -> (Operand -> Operand -> Codegen (I.Type, Operand))
            -> Operand -> Operand -> Codegen (I.Type, Operand)
genDivOp ty fn leftOp rightOp = do
  condOp <- instr boolean $ AST.ICmp IP.EQ rightOp (constZero $ typeToLLVM ty) []
  divB <- addBlock "div"
  exB <- addBlock "div.ex"
  cbr condOp exB divB

  setBlock exB
  apiProcCall CallDivideByZeroEx []
  unreachable

  setBlock divB
  fn leftOp rightOp

-- | Generate integer arithmetic operations with overflow check.
--
-- TODO Make check optional
-- TODO Adjust weights of branches
genArithCall :: I.Type -> StandardCall -> Operand -> Operand -> Codegen (I.Type, Operand)
genArithCall ty c leftOp rightOp = do
  op <- apiFunCall c [leftOp, rightOp]
  obit <- instr boolean $ ExtractValue op [1] []
  arithB <- addBlock "arith"
  exB <- addBlock "arith.ex"
  cbr obit exB arithB

  setBlock exB
  apiProcCall CallIntegerOverflowEx []
  unreachable

  setBlock arithB
  res <- instr (typeToLLVM ty) $ ExtractValue op [0] []
  return (ty, res)

checkBoolean :: CodegenError -> (I.Type, Operand) -> Codegen Operand
checkBoolean _ (I.BooleanType, op) = return op
checkBoolean err _ = throwLocatedError err
