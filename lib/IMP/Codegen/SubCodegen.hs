{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Gode generation for subroutines.

module IMP.Codegen.SubCodegen
    ( SubCodegen
    , getLoopExitBlock
    , execSubCodegen
    , addBlock
    , alloca
    , store
    , load
    , defineLocalVar
    , setExitBlock
    , setBlock
    , br
    , cbr
    , ret
    , unreachable
    , entry
    , callFun
    , callProc
    , withLoopExit
    , getVar
    , apiFunCall
    , apiProcCall
    , exit
    , notInstr
    , instr
    ) where

import IMP.Codegen.Utils

import qualified IMP.AST as I
import qualified IMP.SymbolTable as Tab
import IMP.SourceLoc
import IMP.Codegen.GlobalCodegen
import IMP.Codegen.Error
import qualified LLVM.AST as AST
import LLVM.AST hiding (functionAttributes, metadata)
import qualified LLVM.AST.CallingConvention as CC
import qualified Data.Map.Strict as Map
import qualified LLVM.AST.FunctionAttribute as FA
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List
import Data.Function
import Text.Printf

type Names = Map.Map String Int

data CodegenEnv = CodegenEnv
                { location :: SourcePos
                , loopExitBlock :: Maybe Name
                -- ^ Exit block for innermost loop
                } deriving Show

data CodegenState = CodegenState
                  { currentBlock :: Name
                  -- ^ Name of the active block to append to
                  , exitBlock :: Maybe Name
                  -- ^ Block containing return
                  , subReturn :: Maybe (I.Type, Operand)
                  -- ^ Subroutine return type and location
                  , blocks :: Map.Map Name BlockState
                  -- ^ Blocks of function
                  , symtab :: SymbolTable
                  -- ^ Function scope symbol table
                  , blockCount :: Int
                  -- ^ Count of basic blocks
                  , count :: Word
                  -- ^ Count of unnamed instructions
                  , names :: Names
                  -- ^ Name supply
                  } deriving Show

data BlockState = BlockState
                { idx :: Int
                -- ^ Block index
                , stack :: [Named Instruction]
                -- ^ Stack of unstructions
                , term :: Maybe (Named Terminator)
                -- ^ Block terminator
                } deriving Show

emptyBlock :: Int -> BlockState
emptyBlock ix = BlockState ix [] Nothing

newtype SubCodegen a = SubCodegen
                     { runSubCodegen :: ReaderT CodegenEnv (StateT CodegenState GlobalCodegen) a
                     } deriving ( Functor
                                , Applicative
                                , Monad
                                , MonadError (Located CodegenError)
                                , MonadReader CodegenEnv
                                , MonadState CodegenState
                                )

instance MonadLoc SubCodegen where
  withLoc f x = local (\e -> e { location = getLoc x }) $ f $ unLoc x
  currentLoc = reader location

liftG :: GlobalCodegen a -> SubCodegen a
liftG = SubCodegen . lift . lift

instance MonadCodegen SubCodegen where
  emitString = liftG . emitString
  useStdCall = liftG . useStdCall

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: MonadCodegen m => CodegenState -> m [BasicBlock]
createBlocks = traverse makeBlock . sortBlocks . Map.toList . blocks

makeBlock :: MonadCodegen m => (Name, BlockState) -> m BasicBlock
makeBlock (l, BlockState _ s t) =
  case t of
    Just term -> return $ BasicBlock l (reverse s) term
    Nothing -> throwLocatedError $ InternalError $ printf "Block has no terminator: '%s'."  (show l)

newCodegenState :: GlobalCodegen CodegenState
newCodegenState = do
  syms <- getSymtab

  return CodegenState
              { currentBlock = mkName ""
              , exitBlock = Nothing
              , subReturn = Nothing
              , blocks = Map.empty
              , symtab = Tab.newScope syms
              , blockCount = 1
              , count = 0
              , names = Map.empty
              }

newCodegenEnv :: GlobalCodegen CodegenEnv
newCodegenEnv = do
  pos <- currentLoc
  return CodegenEnv
                { loopExitBlock = Nothing
                , location = pos
                }

execSubCodegen :: SubCodegen a -> GlobalCodegen [BasicBlock]
execSubCodegen m = do
  env <- newCodegenEnv
  s <- newCodegenState
  s' <- execStateT (runReaderT (runSubCodegen m) env) s
  createBlocks s'

withLoopExit :: Name -> SubCodegen a -> SubCodegen a
withLoopExit newExitB = local (\e -> e { loopExitBlock = Just newExitB })

entry :: SubCodegen Name
entry = gets currentBlock

exit :: SubCodegen (Name, Maybe (I.Type, Operand))
exit = do
  b <- gets exitBlock
  case b of
    Just bname -> do
      t <- gets subReturn
      return (bname, t)
    Nothing -> throwLocatedError $ InternalError "Exit block was not set."

addBlock :: String -> SubCodegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (mkName qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return $ mkName qname

setBlock :: Name -> SubCodegen ()
setBlock bname = modify $ \s -> s { currentBlock = bname }

setExitBlock :: Maybe (I.Type, Operand) -> Name -> SubCodegen ()
setExitBlock ty bname =
  modify $ \s -> s { subReturn = ty, exitBlock = Just bname }

modifyBlock :: BlockState -> SubCodegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: SubCodegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> throwLocatedError $ InternalError $ printf "No such block: '%s'" (show c)

fresh :: SubCodegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

defineLocalVar :: I.ID -> I.Type -> Operand -> SubCodegen ()
defineLocalVar name ty x = do
  syms <- gets symtab
  case Tab.insert name (SymbolVariable ty, x) syms of
    Left _ ->
      throwLocatedError $ LocalRedefinition name
    Right syms' ->
      modify $ \s -> s { symtab = syms' }

getVar :: I.ID -> SubCodegen (SymbolType, Operand)
getVar var = do
  syms <- gets symtab
  case Tab.lookup var syms of
    Just x -> return x
    Nothing -> throwLocatedError $ SymbolNotInScope var

instr :: Type -> Instruction -> SubCodegen Operand
instr ty ins = do
  ref <- UnName <$> fresh
  namedInstr ref ty ins

instr' :: String -> Type -> Instruction -> SubCodegen Operand
instr' n = namedInstr (mkName n)

namedInstr :: Name -> Type -> Instruction -> SubCodegen Operand
namedInstr ref ty ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i})
  return $ LocalReference ty ref

voidInstr :: Instruction -> SubCodegen ()
voidInstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = Do ins : i})

alloca :: String -> Type -> SubCodegen Operand
alloca n ty = instr' n ty $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> SubCodegen ()
store ptr val =
  voidInstr $ Store False ptr val Nothing 0 []

notInstr :: Operand -> SubCodegen Operand
notInstr op =
  instr boolean $ AST.Xor constTrue op []

load :: Type -> Operand -> SubCodegen Operand
load ty ptr =
  instr ty $ Load False ptr Nothing 0 []

callFun :: Type -> Operand -> [Operand] -> [FA.FunctionAttribute] -> SubCodegen Operand
callFun retty fun args attrs =
  instr retty $ Call Nothing CC.C [] (Right fun) (zip args (repeat [])) (Right <$> attrs) []

callProc :: Operand -> [Operand] -> [FA.FunctionAttribute] -> SubCodegen ()
callProc fun args attrs =
  voidInstr $ Call Nothing CC.C [] (Right fun) (zip args (repeat [])) (Right <$> attrs) []

terminator :: Named Terminator -> SubCodegen ()
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })

br :: Name -> SubCodegen ()
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> SubCodegen ()
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe Operand -> SubCodegen ()
ret val = terminator $ Do $ Ret val []

unreachable :: SubCodegen ()
unreachable = terminator $ Do $ Unreachable []

apiFunCall :: StandardCall -> [Operand] -> SubCodegen Operand
apiFunCall c args = do
  useStdCall c
  callFun retty op args attrs
 where
  retty = stdCallType c
  op = stdCallOp c
  attrs = stdCallAttrs c

apiProcCall :: StandardCall -> [Operand] -> SubCodegen ()
apiProcCall c args = do
  useStdCall c
  callProc op args attrs
 where
  op = stdCallOp c
  attrs = stdCallAttrs c

getLoopExitBlock :: SubCodegen (Maybe Name)
getLoopExitBlock = reader loopExitBlock
