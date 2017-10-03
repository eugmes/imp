{-# LANGUAGE OverloadedStrings #-}

module IMP.Emit (codegenProgram) where

import qualified IMP.AST as I
import IMP.AST (getID)
import IMP.Codegen
import IMP.SourceLoc
import qualified LLVM.AST as AST
import LLVM.AST hiding (type')
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import Control.Monad.State

codegenProgram :: I.Program -> LLVM ()
codegenProgram (I.Program vars subs) = do
    mapM_ codegenVars vars
    mapM_ codegenSubDecl subs
    mapM_ codegenSub subs
    finalizeLLVM

codegenVars :: I.VarDec -> LLVM ()
codegenVars (I.VarDec names t) = mapM_ (defineVar $ unLoc t) names

toSig :: [I.ParamList] -> [(I.Type, Located I.ID)]
toSig = concatMap toSig'
  where
    toSig' (I.ParamList ids ty) = map (toSig'' $ unLoc ty) ids
    toSig'' t name = (t, name)

codegenSubDecl :: I.Subroutine -> LLVM ()
codegenSubDecl (I.Procedure name params _ _) =
    declareProc name $ fst <$> toSig params

codegenSubDecl (I.Function name params retty _ _) =
    declareFun (unLoc retty) name $ fst <$> toSig params

codegenSub' :: Located I.ID -> Maybe I.Type -> [I.ParamList] -> [I.VarDec] -> [I.Statement] -> LLVM ()
codegenSub' name retty params vars body = do
    blocks <- execCodegen cg
    defineSub retty name args blocks
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
            store t var $ local t $ mkName $ getID $ unLoc a
            assign (SymbolVariable ty) a var
        codegenLocals vars
        mapM_ codegenStatement body
        br exit

        setBlock exit
        case retval of
            Nothing -> ret Nothing
            Just (ty, op) -> load (typeToLLVM ty) op >>= ret . Just

codegenSub :: I.Subroutine -> LLVM ()
codegenSub (I.Procedure name params vars body) = do
    when ((unLoc name == I.ID "main") && not (null params)) $
        error "main should be a procedure with no arguments"
    codegenSub' name Nothing params vars body

codegenSub (I.Function name params retty vars body) = do
    when (unLoc name == I.ID "main") $ error "main should be a procedure"
    codegenSub' name (Just $ unLoc retty) params vars body

codegenLocals :: [I.VarDec] -> Codegen ()
codegenLocals = mapM_ codegenLocals'

codegenLocals' :: I.VarDec -> Codegen ()
codegenLocals' (I.VarDec names ty) = mapM_ (codegenLocal $ unLoc ty) names

codegenLocal :: I.Type -> Located I.ID -> Codegen ()
codegenLocal ty name = do
    var <- alloca (getID $ unLoc name) $ typeToLLVM ty
    assign (SymbolVariable ty) name var

typeCheck :: I.Type -> I.Type -> Codegen ()
typeCheck lt rt =
    when (lt /= rt) $
        error $ "Types don't match: " ++ show (lt, rt)

maybeGenBlock :: String -> Name -> [I.Statement] -> Codegen Name
maybeGenBlock _ contName [] = return contName

-- TODO add block stack
maybeGenBlock newTemlate contName stmts = do
    e <- entry
    newName <- addBlock newTemlate
    setBlock newName
    mapM_ codegenStatement stmts
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

codegenSubCall :: Operand -> Type -> [I.Type] -> [I.Expression] -> Codegen Operand
codegenSubCall name rty args exps = do
    when (length args /= length exps) $
        error "Invalid number of arguments"
    vals <- mapM codegenExpression exps
    forM_ (zip args (map fst vals)) $ uncurry typeCheck
    call rty name (map snd vals)

codegenStatement :: I.Statement -> Codegen ()

codegenStatement (I.IfStatement cond ifTrue ifFalse) = do
    (ty, op) <- codegenExpression cond
    case ty of
        I.BooleanType -> do
            ifExitB <- addBlock "if.exit"
            ifTrueB <- maybeGenBlock "if.true" ifExitB ifTrue
            ifFalseB <- maybeGenBlock "if.false" ifExitB ifFalse

            cbr op ifTrueB ifFalseB

            setBlock ifExitB
        _ -> error "Boolean expression expected as if statement codition"

codegenStatement (I.WhileStatement cond body) = do
    whileCondB <- addBlock "while.cond"
    whileExitB <- addBlock "while.exit"
    br whileCondB

    setBlock whileCondB

    (ty, op) <- codegenExpression cond
    case ty of
        I.BooleanType -> do
            whileLoopB <- withLoopExit whileExitB $ maybeGenBlock "while.loop" whileCondB body
            cbr op whileLoopB whileExitB

            setBlock whileExitB
        _ -> error "Boolean expression expected as while statement condition"

codegenStatement (I.AssignStatement name exp) = do
    (varSymType, varPtr) <- getvar $ unLoc name
    varType <- case varSymType of
        SymbolVariable ty -> return ty
        _ -> error $ show name ++ " is not a variable"
    (expType, op) <- codegenExpression exp
    typeCheck varType expType
    store (typeToLLVM varType) varPtr op

codegenStatement (I.CallStatement name exps) = do
    (ty, proc) <- getvar $ unLoc name
    case ty of
        SymbolVariable _ -> error "Attempt to call a variable"
        SymbolFunction _ _ -> error "Attempt to call a function as procedure"
        SymbolProcedure args -> void $ codegenSubCall proc Type.void args exps

codegenStatement (I.InputStatement name) = do
    (ty, ptr) <- getvar $ unLoc name
    case ty of
        SymbolVariable t -> do
            op <- case t of
                I.IntegerType -> apiCall CallInputInteger []
                _ -> error "Attempt to input something other than integer"
            store (typeToLLVM t) ptr op
        _ -> error "Attempt to input a subroutine"

codegenStatement (I.OutputStatement (I.Exp exp)) = do
    (ty, op) <- codegenExpression exp
    let api = case ty of
              I.IntegerType -> CallOutputInteger
              I.BooleanType -> CallOutputBoolean
    void $ apiCall api [op]

codegenStatement (I.OutputStatement (I.Str str)) = do
    opPtr <- newString $ unLoc str
    void $ apiCall CallOutputString [opPtr]

codegenStatement I.NullStatement = return ()

codegenStatement I.BreakStatement = do
    loopEx <- gets loopExitBlock
    case loopEx of
        Nothing -> error "break can only be used inside a loop"
        Just bname -> gotoBlock bname

codegenStatement I.ReturnStatement = do
    ex <- exit
    case ex of
        (_, Just _) -> error "Function must return a value"
        (bname, Nothing) -> gotoBlock bname

codegenStatement (I.ReturnValStatement exp) = do
    ex <- exit
    case ex of
        (bname, Just (retty, ptr)) -> do
            (ty, op) <- codegenExpression exp
            typeCheck retty ty
            store (typeToLLVM retty) ptr op
            gotoBlock bname
        (_, Nothing) -> error "Procedure cannot return a value"

codegenStatement I.HaltStatement = void $ apiCall CallHalt []
codegenStatement I.NewlineStatement = void $ apiCall CallNewline []

codegenExpression :: I.Expression -> Codegen (I.Type, Operand)
codegenExpression (I.UnOpExp unaryOp expr) = do
    (ty, fOp) <- codegenExpression expr
    op <- case (unaryOp, ty) of
        (I.OpNot, I.BooleanType) ->
            notInstr fOp
        (I.OpNeg, I.IntegerType) ->
            negateInstr (typeToLLVM ty) fOp
        _ -> error $ "Invalid type for unary operator: " ++ show (unaryOp, ty)
    return (ty, op)

codegenExpression (I.BinOpExp leftExp binaryOp rightExp) = do
    (leftType, leftOp) <- codegenExpression leftExp
    (rightType, rightOp) <- codegenExpression rightExp
    typeCheck leftType rightType

    let fn = case (binaryOp, leftType) of
               (I.OpEQ, _) -> icmp IP.EQ
               (I.OpLT, I.IntegerType) -> icmp IP.SLT
               (I.OpLE, I.IntegerType) -> icmp IP.SLE
               (I.OpGT, I.IntegerType) -> icmp IP.SGT
               (I.OpGE, I.IntegerType) -> icmp IP.SLE
               (I.OpNE, _) -> icmp IP.NE
               (I.OpAdd, I.IntegerType) -> add
               (I.OpSub, I.IntegerType) -> sub
               (I.OpOr, I.BooleanType) -> or
               (I.OpMul, I.IntegerType) -> mul
               (I.OpDiv, I.IntegerType) -> genDivOp leftType sdiv
               (I.OpMod, I.IntegerType) -> genDivOp leftType srem
               (I.OpAnd, I.BooleanType) -> and
               p -> error $ "Invalid type for binary operation: " ++ show p

    fn leftOp rightOp
  where
    wrap ty f op0 op1 = do
        op <- instr (typeToLLVM ty) $ f op0 op1 []
        return (ty, op)

    icmp pred = wrap I.BooleanType (AST.ICmp pred)
    add = wrap I.IntegerType $ AST.Add True False
    sub = wrap I.IntegerType $ AST.Sub True False
    or = wrap I.BooleanType AST.Or
    mul = wrap I.IntegerType $ AST.Mul True False
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
    (ty, ptr) <- getvar $ unLoc name
    case ty of
        SymbolVariable t -> do
            op <- load (typeToLLVM t) ptr
            return (t, op)
        _ -> error "Attempt to read value of a subroutine"

codegenExpression (I.CallExpression name exps) = do
    (ty, fun) <- getvar $ unLoc name
    case ty of
        SymbolVariable _ -> error "Attempt to call a variable"
        SymbolProcedure _ -> error "Attempt to call a procedure as function"
        SymbolFunction rtype args -> do
            op <- codegenSubCall fun (typeToLLVM rtype) args exps
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
  void $ apiCall CallDivideByZeroEx []
  unreachable

  setBlock divB
  fn leftOp rightOp
