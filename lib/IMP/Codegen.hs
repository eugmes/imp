{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module IMP.Codegen
    ( LLVM
    , StandardCall (..)
    , Codegen
    , SymbolType (..)
    , loopExitBlock
    , execLLVM
    , emptyModule -- TODO unexport this
    , finalizeLLVM
    , defineVar
    , declareProc
    , declareFun
    , execCodegen
    , defineSub
    , addBlock
    , entryBlockName -- TODO unexport those
    , exitBlockName
    , typeToLLVM
    , alloca
    , store
    , load
    , local
    , assign
    , setExitBlock
    , setBlock
    , br
    , cbr
    , ret
    , unreachable
    , entry
    , call
    , withLoopExit
    , getvar
    , apiCall
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
import qualified LLVM.AST.Type as Type
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
stdCallType _ = Type.void

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

type SymbolTableEntry = (SymbolType, Operand)

type SymbolTable = Tab.SymbolTable I.ID SymbolTableEntry

type Names = Map.Map String Int

data CodegenState = CodegenState
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

newtype Codegen a = Codegen
                  { runCodegen :: StateT CodegenState (Except CodegenError) a
                  } deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadError CodegenError
                             , MonadState CodegenState
                             )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> LLVM [BasicBlock]
createBlocks = traverse makeBlock . sortBlocks . Map.toList . blocks

makeBlock :: (Name, BlockState) -> LLVM BasicBlock
makeBlock (l, BlockState _ s t) =
  case t of
    Just term -> return $ BasicBlock l (reverse s) term
    Nothing -> throwError $ InternalError $ printf "Block has no terminator: '%s'."  (show l)

entryBlockName, exitBlockName :: String
entryBlockName = "entry"
exitBlockName = "exit"

newCodegen :: LLVM CodegenState
newCodegen = do
    syms <- gets globalSymtab
    strCount <- gets globalStringsCount
    strs <- gets globalStrings -- TODO check if this is really needed
    usedCalls <- gets globalUsedCalls

    return CodegenState
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
                }

execCodegen :: Codegen a -> LLVM [BasicBlock]
execCodegen m = do
    cg <- newCodegen
    let res = runExcept $ execStateT (runCodegen m) cg
    case res of
      Left err -> throwError err
      Right cgen -> do
        modify $ \s -> s { globalStringsCount = stringCount cgen
                         , globalStrings = strings cgen
                         , globalUsedCalls = apis cgen
                         }
        createBlocks cgen

withLoopExit :: Name -> Codegen a -> Codegen a
withLoopExit newExitB m = do
    oldExitB <- gets loopExitBlock
    modify $ \s -> s { loopExitBlock = Just newExitB }
    r <- m
    modify $ \s -> s { loopExitBlock = oldExitB }
    return r

data LLVMState = LLVMState
               { currentModule :: AST.Module
               , globalSymtab :: SymbolTable
               , globalStringsCount :: Word
               , globalStrings :: Map.Map Name U8.ByteString
               , globalUsedCalls :: Set.Set StandardCall
               , globalMetadataCount :: Word
               } deriving Show

newtype LLVM a = LLVM { runLLVM :: StateT LLVMState (Except CodegenError) a }
    deriving (Functor, Applicative, Monad, MonadState LLVMState, MonadError CodegenError)

newLLVMState :: AST.Module -> LLVMState
newLLVMState m = LLVMState m Tab.empty 0 Map.empty Set.empty 0

execLLVM :: AST.Module -> LLVM a -> Either CodegenError AST.Module
execLLVM md m = fmap currentModule $ runExcept $ execStateT (runLLVM m) (newLLVMState md)

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
addSym :: SymbolType -> Type -> Located I.ID -> LLVM ()
addSym st lt n = do
    syms <- gets globalSymtab
    let sym = (st, ConstantOperand $ GlobalReference lt (mkName $ getID $ unLoc n))
    case Tab.insert (unLoc n) sym syms of
        Left _ -> throwError $ GlobalRedefinition (unLoc n)
        Right syms' -> modify $ \s -> s { globalSymtab = syms' }

-- | Add global definition
addDefn :: Definition -> LLVM ()
addDefn d = do
    m <- gets currentModule

    let defs = moduleDefinitions m
    modify $ \s -> s { currentModule = m { moduleDefinitions = defs ++ [d] }}

-- | Declares function in global symbol table
declareFun :: I.Type -> Located I.ID -> [I.Type] -> LLVM ()
declareFun retty label argtys = addSym symt t label
  where
    symt = SymbolFunction retty argtys
    t = typeToLLVM retty

-- | Declares procedure in global symbol table
declareProc :: Located I.ID -> [I.Type] -> LLVM ()
declareProc label argtys = addSym symt Type.void label
  where
    symt = SymbolProcedure argtys

-- | Adds global function definition
defineSub :: Maybe I.Type -> Located I.ID -> [(I.Type, Located I.ID)] -> [BasicBlock] -> LLVM ()
defineSub retty label argtys body = addDefn def
  where
    t = maybe Type.void typeToLLVM retty
    def = GlobalDefinition $
            functionDefaults { name = (mkName . getID . unLoc) label
                             , parameters = ([Parameter (typeToLLVM ty) ((mkName . getID . unLoc)  nm) []
                                                     | (ty, nm) <- argtys], False)
                             , returnType = t
                             , basicBlocks = body
                             }

-- | Add global variable definition
--
-- Also adds this variable to symbol table
defineVar :: I.Type -> Located I.ID -> LLVM ()
defineVar ty label = addSym (SymbolVariable ty) t label >> addDefn def
  where
    n = mkName $ getID $ unLoc label
    t = typeToLLVM ty
    def = GlobalDefinition $ globalVariableDefaults { name = n, type' = t }

entry :: Codegen Name
entry = gets currentBlock

exit :: Codegen (Name, Maybe (I.Type, Operand))
exit = do
    b <- gets exitBlock
    case b of
        Just bname -> do
            t <- gets subReturn
            return (bname, t)
        Nothing -> throwError $ InternalError "Exit block was not set."

addBlock :: String -> Codegen Name
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

setBlock :: Name -> Codegen ()
setBlock bname = modify $ \s -> s { currentBlock = bname }

setExitBlock :: Maybe (I.Type, Operand) -> Name -> Codegen ()
setExitBlock ty bname =
    modify $ \s -> s { subReturn = ty, exitBlock = Just bname }

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> throwError $ InternalError $ printf "No such block: '%s'" (show c)

fresh :: Codegen Word
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

assign :: SymbolType -> Located I.ID -> Operand -> Codegen ()
assign ty var x = do
    syms <- gets symtab
    case Tab.insert (unLoc var) (ty, x) syms of
        Left _ ->
            throwError $ LocalRedefinition $ unLoc var
        Right syms' ->
            modify $ \s -> s { symtab = syms' }

getvar :: I.ID -> Codegen (SymbolType, Operand)
getvar var = do
    syms <- gets symtab
    case Tab.lookup var syms of
        Just x -> return x
        Nothing -> throwError $ SymbolNotInScope var

instr :: Type -> Instruction -> Codegen Operand
instr ty ins = do
    ref <- UnName <$> fresh
    namedInstr ref ty ins

instr' :: String -> Type -> Instruction -> Codegen Operand
instr' n = namedInstr (mkName n)

namedInstr :: Name -> Type -> Instruction -> Codegen Operand
namedInstr ref ty ins = do
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (ref := ins) : i})
    return $ local ty ref

alloca :: String -> Type -> Codegen Operand
alloca n ty = instr' n ty $ Alloca ty Nothing 0 []

store :: Type -> Operand -> Operand -> Codegen ()
store ty ptr val =
    void $ instr ty $ Store False ptr val Nothing 0 []

notInstr :: Operand -> Codegen Operand
notInstr op =
    instr boolean $ AST.Xor constTrue op []

load :: Type -> Operand -> Codegen Operand
load ty ptr =
    instr ty $ Load False ptr Nothing 0 []

call :: Type -> Operand -> [Operand] -> Codegen Operand
call ty fun args =
    instr ty $ Call Nothing CC.C [] (Right fun) (zip args (repeat [])) [] []

terminator :: Named Terminator -> Codegen ()
terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm })

br :: Name -> Codegen ()
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen ()
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe Operand -> Codegen ()
ret val = terminator $ Do $ Ret val []

unreachable :: Codegen ()
unreachable = terminator $ Do $ Unreachable []

newString :: String -> Codegen Operand
newString s = do
    n <- gets stringCount
    strs <- gets strings
    let newName = mkName $ ".str." ++ show n
    modify $ \s -> s { stringCount = n + 1, strings = Map.insert newName u8s strs }

    let sz = B.length u8s + 1
        ty = ArrayType (fromIntegral sz) i8
        addr = GlobalReference ty newName
        ptr = ConstantOperand $ C.GetElementPtr True addr [ C.Int (typeBits integer) 0
                                                          , C.Int (typeBits integer) 0 ]
    return ptr
  where
    u8s = fromString s

emitStrings :: LLVM ()
emitStrings = do
    strings <- Map.toList <$> gets globalStrings
    mapM_ emitString strings

emitString :: (Name, U8.ByteString) -> LLVM ()
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

emitStdCalls :: LLVM ()
emitStdCalls = do
    apis <- Set.toList <$> gets globalUsedCalls
    mapM_ emitStdCall apis

emitStdCall :: StandardCall -> LLVM ()
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

apiCall :: StandardCall -> [Operand] -> Codegen Operand
apiCall c args = do
    used <- gets apis
    modify $ \s -> s { apis = Set.insert c used }
    instr ty $ Call Nothing CC.C [] (Right fun) (zip args (repeat [])) attrs []
  where
    ty = stdCallType c
    fun = ConstantOperand $ GlobalReference ty $ stdCallName c
    attrs = Right <$> stdCallAttrs c

namedMetadata :: ShortByteString -> [MetadataNodeID] -> LLVM ()
namedMetadata name ids = addDefn $ NamedMetadataDefinition name ids

newMetadataNodeID :: LLVM MetadataNodeID
newMetadataNodeID = do
  metadataCount <- gets globalMetadataCount
  modify $ \s -> s { globalMetadataCount = metadataCount + 1 }
  return $ MetadataNodeID metadataCount

metadata :: [Maybe Metadata] -> LLVM MetadataNodeID
metadata defs = do
  nd <- newMetadataNodeID
  addDefn $ MetadataNodeDefinition nd defs
  return nd

emitCompilerInfo :: LLVM ()
emitCompilerInfo = do
  nd <- metadata [Just $ MDString $ fromString $ "IMP version " ++ showVersion version]
  namedMetadata "llvm.ident" [nd]

finalizeLLVM :: LLVM ()
finalizeLLVM = do
  emitStrings
  emitStdCalls
  emitCompilerInfo
