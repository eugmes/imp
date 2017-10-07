{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module IMP.Codegen.GlobalCodegen
    ( MonadCodegen(..)
    , GlobalCodegen
    , CodegenOptions(..)
    , getSymtab
    , execGlobalCodegen
    , defineVar
    , declareProc
    , declareFun
    , defineSub
    ) where

import Paths_imp
import IMP.Codegen.Utils

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
import LLVM.AST.Linkage
import qualified Data.Set as Set
import LLVM.AST.DataLayout
import LLVM.Prelude (ShortByteString)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.String
import Data.Version
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString as B

class (MonadError (Located CodegenError) m, MonadLoc m) => MonadCodegen m where
  emitString :: String -> m Operand
  useStdCall :: StandardCall -> m ()

data CodegenOptions = CodegenOptions
                    { sourceFileName :: FilePath
                    , dataLayout :: DataLayout
                    , targetTriple :: ShortByteString
                    } deriving Show

newtype CodegenEnv = CodegenEnv
                   { location :: SourcePos
                   } deriving Show

data CodegenState = CodegenState
                  { currentModule :: AST.Module
                  , symtab :: SymbolTable
                  , nextStringNum :: Word
                  , usedCalls :: Set.Set StandardCall
                  , nextMetadataNum :: Word
                  } deriving Show

newtype GlobalCodegen a = GlobalCodegen
                        { runGlobalCodegen :: ReaderT CodegenEnv (StateT CodegenState (Except (Located CodegenError))) a
                        } deriving ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadReader CodegenEnv
                                   , MonadState CodegenState
                                   , MonadError (Located CodegenError))

instance MonadLoc GlobalCodegen where
  withLoc f x = local (\e -> e { location = getLoc x }) $ f $ unLoc x
  currentLoc = reader location

instance MonadCodegen GlobalCodegen where
  emitString = globalEmitString
  useStdCall = globalUseStdCall

execGlobalCodegen :: CodegenOptions -> GlobalCodegen a -> Either (Located CodegenError) AST.Module
execGlobalCodegen opts m =
  fmap currentModule $ runExcept $ execStateT (runReaderT (runGlobalCodegen m') env) s
 where
  m' = m >> emitCompilerInfo
  md = emptyModule opts
  s = CodegenState md Tab.empty 0 Set.empty 0
  env = CodegenEnv { location = initialPos $ sourceFileName opts }

emptyModule :: CodegenOptions -> AST.Module
emptyModule opts =
  defaultModule
    { moduleName = fromString $ sourceFileName opts
    , moduleSourceFileName = fromString $ sourceFileName opts
    , moduleDataLayout = Just $ dataLayout opts
    , moduleTargetTriple = Just $ targetTriple opts
    }

getSymtab :: GlobalCodegen SymbolTable
getSymtab = gets symtab

-- | Add symbol to the global symbol table
--
-- TODO Insert location information into symbol table
addSym :: SymbolType -> Type -> I.ID -> GlobalCodegen ()
addSym st lt n = do
    syms <- gets symtab
    let sym = (st, ConstantOperand $ GlobalReference lt (mkName $ getID n))
    case Tab.insert n sym syms of
        Left _ -> throwLocatedError $ GlobalRedefinition n
        Right syms' -> modify $ \s -> s { symtab = syms' }

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

newStringName :: GlobalCodegen Name
newStringName = do
  n <- gets nextStringNum
  modify $ \s -> s { nextStringNum = n + 1 }
  return $ mkName $ ".str." ++ show n

globalEmitString :: String -> GlobalCodegen Operand
globalEmitString s = do
  name <- newStringName
  let d = GlobalDefinition $
          globalVariableDefaults { name = name
                                 , linkage = Private
                                 , unnamedAddr = Just GlobalAddr
                                 , isConstant = True
                                 , type' = ArrayType { nArrayElements = size
                                                     , elementType = i8 }
                                 , initializer = Just ini }
      ty = ptr $ ArrayType (fromIntegral size) i8
      addr = GlobalReference ty name
      op = ConstantOperand $ C.GetElementPtr True addr [ C.Int (typeBits integer) 0
                                                       , C.Int (typeBits integer) 0 ]
  addDefn d
  return op
 where
  content :: U8.ByteString
  content = fromString s
  vals = map (C.Int 8 . fromIntegral) (B.unpack content ++ [0])
  size = fromIntegral $ length vals
  ini = C.Array i8 vals

globalUseStdCall :: StandardCall -> GlobalCodegen ()
globalUseStdCall c = do
  used <- gets usedCalls
  unless (Set.member c used) $ do
    modify $ \s -> s { usedCalls = Set.insert c used }
    emitStdCallDecl c

emitStdCallDecl :: StandardCall -> GlobalCodegen ()
emitStdCallDecl c = addDefn d
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

namedMetadata :: ShortByteString -> [MetadataNodeID] -> GlobalCodegen ()
namedMetadata name ids = addDefn $ NamedMetadataDefinition name ids

newMetadataNodeID :: GlobalCodegen MetadataNodeID
newMetadataNodeID = do
  n <- gets nextMetadataNum
  modify $ \s -> s { nextMetadataNum = n + 1 }
  return $ MetadataNodeID n

metadata :: [Maybe Metadata] -> GlobalCodegen MetadataNodeID
metadata defs = do
  nd <- newMetadataNodeID
  addDefn $ MetadataNodeDefinition nd defs
  return nd

emitCompilerInfo :: GlobalCodegen ()
emitCompilerInfo = do
  nd <- metadata [Just $ MDString $ fromString $ "IMP version " ++ showVersion version]
  namedMetadata "llvm.ident" [nd]
