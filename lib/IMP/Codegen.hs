{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module IMP.Codegen
    ( GlobalCodegen
    , StandardCall (..)
    , SubCodegen
    , SymbolType (..)
    , loopExitBlock
    , execGlobalCodegen
    , emptyModule -- TODO unexport this
    , finalizeGlobalCodegen
    , defineVar
    , declareProc
    , declareFun
    , execSubCodegen
    , defineSub
    , addBlock
    , entryBlockName -- TODO unexport those
    , exitBlockName
    , typeToLLVM
    , alloca
    , store
    , load
    , local
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
    , newString
    , exit
    , notInstr
    , instr
    , integer
    , constTrue
    , constFalse
    , constZero
    , boolean
    ) where

import Paths_imp
import qualified IMP.AST as I
import IMP.AST (getID)
import qualified IMP.SymbolTable as Tab
import IMP.SourceLoc
import IMP.Codegen.Error
import qualified LLVM.AST as AST
import LLVM.AST hiding (type', functionAttributes, metadata)
import LLVM.AST.Type hiding (void)
import LLVM.AST.Global
import LLVM.AST.Constant hiding (type')
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Linkage
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout
import LLVM.Prelude (ShortByteString)
import qualified LLVM.AST.FunctionAttribute as FA
import Control.Monad.State
import Control.Monad.Except
import Data.String
import Data.List
import Data.Function
import Data.Version
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString as B
import Text.Printf

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

type Names = Map.Map String Int

data SubCodegenState = SubCodegenState
                     { currentBlock :: Name
                     -- |^ Name of the active block to append to
                     , exitBlock :: Maybe Name
                     -- |^ Block containing return
                     , loopExitBlock :: Maybe Name
                     -- |^ Exit block for innermost loop
                     , subReturn :: Maybe (I.Type, Operand)
                     -- |^ Subroutine return type and location
                     , blocks :: Map.Map Name BlockState
                     -- |^ Blocks of function
                     , symtab :: SymbolTable
                     -- |^ Function scope symbol table
                     , blockCount :: Int
                     -- |^ Count of basic blocks
                     , count :: Word
                     -- |^ Count of unnamed instructions
                     , names :: Names
                     -- |^ Name supply
                     , stringCount :: Word
                     -- |^ Count of emitted strings
                     , strings :: Map.Map Name U8.ByteString
                     -- |^ Emitted strings
                     , apis :: Set.Set StandardCall
                     -- |^ Used standard calls
                     , currentLocation :: SourcePos
                     } deriving Show

data BlockState = BlockState
                { idx :: Int
                -- |^ Block index
                , stack :: [Named Instruction]
                -- |^ Stack of unstructions
                , term :: Maybe (Named Terminator)
                -- |^ Block terminator
                } deriving Show

emptyBlock :: Int -> BlockState
emptyBlock ix = BlockState ix [] Nothing

newtype SubCodegen a = SubCodegen
                     { runSubCodegen :: StateT SubCodegenState (Except (Located CodegenError)) a
                     } deriving ( Functor
                                , Applicative
                                , Monad
                                , MonadError (Located CodegenError)
                                , MonadState SubCodegenState
                                )

instance MonadLoc SubCodegen where
  withLoc f x = do
    oldLoc <- gets currentLocation
    modify $ \s -> s { currentLocation = getLoc x }
    r <- f $ unLoc x
    modify $ \s -> s { currentLocation = oldLoc }
    return r

  currentLoc = gets currentLocation

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

type MonadCodegenError m = (MonadLoc m, MonadError (Located CodegenError) m)

createBlocks :: MonadCodegenError m => SubCodegenState -> m [BasicBlock]
createBlocks = traverse makeBlock . sortBlocks . Map.toList . blocks

makeBlock :: MonadCodegenError m => (Name, BlockState) -> m BasicBlock
makeBlock (l, BlockState _ s t) =
  case t of
    Just term -> return $ BasicBlock l (reverse s) term
    Nothing -> throwLocatedError $ InternalError $ printf "Block has no terminator: '%s'."  (show l)

entryBlockName, exitBlockName :: String
entryBlockName = "entry"
exitBlockName = "exit"

newSubCodegen :: SourcePos -> GlobalCodegen SubCodegenState
newSubCodegen pos = do
    syms <- gets globalSymtab
    strCount <- gets globalStringsCount
    strs <- gets globalStrings -- TODO check if this is really needed
    usedCalls <- gets globalUsedCalls

    return SubCodegenState
                { currentBlock = mkName entryBlockName
                , exitBlock = Nothing
                , loopExitBlock = Nothing
                , subReturn = Nothing
                , blocks = Map.empty
                , symtab = Tab.newScope syms
                , blockCount = 1
                , count = 0
                , names = Map.empty
                , stringCount = strCount
                , strings = strs
                , apis = usedCalls
                , currentLocation = pos
                }

execSubCodegen :: SubCodegen a -> GlobalCodegen [BasicBlock]
execSubCodegen m = do
    pos <- currentLoc
    cg <- newSubCodegen pos
    let res = runExcept $ execStateT (runSubCodegen m) cg
    case res of
      Left err -> throwError err
      Right cgen -> do
        modify $ \s -> s { globalStringsCount = stringCount cgen
                         , globalStrings = strings cgen
                         , globalUsedCalls = apis cgen
                         }
        createBlocks cgen

withLoopExit :: Name -> SubCodegen a -> SubCodegen a
withLoopExit newExitB m = do
    oldExitB <- gets loopExitBlock
    modify $ \s -> s { loopExitBlock = Just newExitB }
    r <- m
    modify $ \s -> s { loopExitBlock = oldExitB }
    return r

data GlobalCodegenState = GlobalCodegenState
               { currentModule :: AST.Module
               , globalSymtab :: SymbolTable
               , globalStringsCount :: Word
               , globalStrings :: Map.Map Name U8.ByteString
               , globalUsedCalls :: Set.Set StandardCall
               , globalMetadataCount :: Word
               , globalLocation :: SourcePos
               } deriving Show

newtype GlobalCodegen a = GlobalCodegen
                        { runGlobalCodegen :: StateT GlobalCodegenState (Except (Located CodegenError)) a
                        } deriving ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadState GlobalCodegenState
                                   , MonadError (Located CodegenError))

instance MonadLoc GlobalCodegen where
  withLoc f x = do
    oldLoc <- gets globalLocation
    modify $ \s -> s { globalLocation = getLoc x }
    r <- f $ unLoc x
    modify $ \s -> s { globalLocation = oldLoc }
    return r

  currentLoc = gets globalLocation

newGlobalCodegenState :: FilePath -> AST.Module -> GlobalCodegenState
newGlobalCodegenState fileName m = GlobalCodegenState m Tab.empty 0 Map.empty Set.empty 0 (initialPos fileName)

execGlobalCodegen :: FilePath -> AST.Module -> GlobalCodegen a -> Either (Located CodegenError) AST.Module
execGlobalCodegen fileName md m =
  fmap currentModule $ runExcept $ execStateT (runGlobalCodegen m) (newGlobalCodegenState fileName md)

emptyModule :: String -> FilePath -> DataLayout -> ShortByteString -> AST.Module
emptyModule label sourceFile dataLayout targetTriple = defaultModule
                                             { moduleName = fromString label
                                             , moduleSourceFileName = fromString sourceFile
                                             , moduleDataLayout = Just dataLayout
                                             , moduleTargetTriple = Just targetTriple
                                             }

integer, boolean, stringType, integerAndBoolean :: Type
integer = i32
boolean = i1
stringType = PointerType i8 $ AddrSpace 0
integerAndBoolean = StructureType False [integer, boolean]

constFalse, constTrue :: Operand
constFalse = ConstantOperand $ C.Int 1 0
constTrue = ConstantOperand $ C.Int 1 1

constZero :: Type -> Operand
constZero ty = ConstantOperand $ C.Int (typeBits ty) 0

typeToLLVM :: I.Type -> Type
typeToLLVM I.IntegerType = integer
typeToLLVM I.BooleanType = boolean

-- | Add symbol to the global symbol table
--
-- TODO Insert location information into symbol table
addSym :: SymbolType -> Type -> I.ID -> GlobalCodegen ()
addSym st lt n = do
    syms <- gets globalSymtab
    let sym = (st, ConstantOperand $ GlobalReference lt (mkName $ getID n))
    case Tab.insert n sym syms of
        Left _ -> throwLocatedError $ GlobalRedefinition n
        Right syms' -> modify $ \s -> s { globalSymtab = syms' }

-- | Add global definition
addDefn :: Definition -> GlobalCodegen ()
addDefn d = do
    m <- gets currentModule

    let defs = moduleDefinitions m
    modify $ \s -> s { currentModule = m { moduleDefinitions = defs ++ [d] }}

-- | Declares function in global symbol table
declareFun :: I.ID -> I.Type -> [I.Type] -> GlobalCodegen ()
declareFun label retty argtys = addSym symt t label
  where
    symt = SymbolFunction retty argtys
    t = ptr $ FunctionType (typeToLLVM retty) (map typeToLLVM argtys) False

-- | Declares procedure in global symbol table
declareProc :: I.ID -> [I.Type] -> GlobalCodegen ()
declareProc label argtys = addSym symt t label
  where
    symt = SymbolProcedure argtys
    t = ptr $ FunctionType VoidType (map typeToLLVM argtys) False

-- | Adds global function definition
defineSub :: I.ID -> Maybe I.Type -> [(I.Type, Located I.ID)] -> [BasicBlock] -> GlobalCodegen ()
defineSub label retty argtys body = addDefn def
  where
    t = maybe VoidType typeToLLVM retty
    def = GlobalDefinition $
            functionDefaults { name = (mkName . getID) label
                             , parameters = ([Parameter (typeToLLVM ty) ((mkName . getID . unLoc)  nm) []
                                                     | (ty, nm) <- argtys], False)
                             , returnType = t
                             , basicBlocks = body
                             }

-- | Add global variable definition
--
-- Also adds this variable to symbol table
defineVar :: I.Type -> I.ID -> GlobalCodegen ()
defineVar ty label = addSym (SymbolVariable ty) (ptr t) label >> addDefn def
  where
    n = mkName $ getID label
    t = typeToLLVM ty
    def = GlobalDefinition $ globalVariableDefaults { name = n, type' = t, initializer = Just $ Undef t }

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

local :: Type -> Name -> Operand
local = LocalReference

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
    return $ local ty ref

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

newString :: String -> SubCodegen Operand
newString s = do
    n <- gets stringCount
    strs <- gets strings
    let newName = mkName $ ".str." ++ show n
    modify $ \s -> s { stringCount = n + 1, strings = Map.insert newName u8s strs }

    let sz = B.length u8s + 1
        ty = ptr $ ArrayType (fromIntegral sz) i8
        addr = GlobalReference ty newName
        op = ConstantOperand $ C.GetElementPtr True addr [ C.Int (typeBits integer) 0
                                                         , C.Int (typeBits integer) 0 ]
    return op
  where
    u8s = fromString s

emitStrings :: GlobalCodegen ()
emitStrings = do
    strings <- Map.toList <$> gets globalStrings
    mapM_ emitString strings

emitString :: (Name, U8.ByteString) -> GlobalCodegen ()
emitString (n, content) = addDefn d
  where
    vals = map (C.Int 8 . fromIntegral) (B.unpack content ++ [0])
    size = fromIntegral $ length vals
    ini = C.Array i8 vals
    d = GlobalDefinition $
        globalVariableDefaults { name = n
                               , linkage = Private
                               , unnamedAddr = Just GlobalAddr
                               , isConstant = True
                               , type' = ArrayType { nArrayElements = size
                                                   , elementType = i8 }
                               , initializer = Just ini }

emitStdCalls :: GlobalCodegen ()
emitStdCalls = do
    apis <- Set.toList <$> gets globalUsedCalls
    mapM_ emitStdCall apis

emitStdCall :: StandardCall -> GlobalCodegen ()
emitStdCall c = addDefn d
  where
    retty = stdCallType c
    n = stdCallName c
    args = stdCallArgs c
    attrs = Right <$> stdCallAttrs c
    d = GlobalDefinition $
        functionDefaults { name = n
                         , parameters = ([Parameter ty "" [] | ty <- args], False)
                         , returnType = retty
                         , functionAttributes = attrs
                         }

apiFunCall :: StandardCall -> [Operand] -> SubCodegen Operand
apiFunCall c args = do
    used <- gets apis
    modify $ \s -> s { apis = Set.insert c used }
    callFun retty op args attrs
  where
    retty = stdCallType c
    op = stdCallOp c
    attrs = stdCallAttrs c

apiProcCall :: StandardCall -> [Operand] -> SubCodegen ()
apiProcCall c args = do
    used <- gets apis
    modify $ \s -> s { apis = Set.insert c used }
    callProc op args attrs
  where
    op = stdCallOp c
    attrs = stdCallAttrs c

namedMetadata :: ShortByteString -> [MetadataNodeID] -> GlobalCodegen ()
namedMetadata name ids = addDefn $ NamedMetadataDefinition name ids

newMetadataNodeID :: GlobalCodegen MetadataNodeID
newMetadataNodeID = do
  metadataCount <- gets globalMetadataCount
  modify $ \s -> s { globalMetadataCount = metadataCount + 1 }
  return $ MetadataNodeID metadataCount

metadata :: [Maybe Metadata] -> GlobalCodegen MetadataNodeID
metadata defs = do
  nd <- newMetadataNodeID
  addDefn $ MetadataNodeDefinition nd defs
  return nd

emitCompilerInfo :: GlobalCodegen ()
emitCompilerInfo = do
  nd <- metadata [Just $ MDString $ fromString $ "IMP version " ++ showVersion version]
  namedMetadata "llvm.ident" [nd]

finalizeGlobalCodegen :: GlobalCodegen ()
finalizeGlobalCodegen = do
  emitStrings
  emitStdCalls
  emitCompilerInfo
