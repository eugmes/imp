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
import IMP.Types
import qualified LLVM.AST as AST
import LLVM.AST hiding (type', functionAttributes, metadata, mkName)
import LLVM.AST.Type hiding (void)
import LLVM.AST.Global hiding (metadata)
import LLVM.AST.Constant hiding (type')
import qualified LLVM.AST.Constant as C
import LLVM.AST.Linkage
import qualified Data.Set as Set
import LLVM.AST.DataLayout
import LLVM.Prelude (ShortByteString)
import Control.Monad.State
import Control.Monad.Except
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString as B
import TextShow (showt)

class (MonadError (Located CodegenError) m, WithLoc m) => MonadCodegen m where
  emitString :: T.Text -> m Operand
  useStdCall :: StandardCall -> m ()

data CodegenOptions = CodegenOptions
                    { sourceFileName :: FilePath
                    , dataLayout :: DataLayout
                    , targetTriple :: ShortByteString
                    } deriving Show

data CodegenState = CodegenState
                  { currentModule :: AST.Module
                  , symtab :: SymbolTable
                  , nextStringNum :: Word
                  , usedCalls :: Set.Set StandardCall
                  , nextMetadataNum :: Word
                  , location :: SourcePos
                  } deriving Show

newtype GlobalCodegen a = GlobalCodegen
                        { runGlobalCodegen :: StateT CodegenState (Except (Located CodegenError)) a
                        } deriving ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadFix
                                   , MonadState CodegenState
                                   , MonadError (Located CodegenError))

instance WithLoc GlobalCodegen where
  withNewLoc pos action = do
    oldPos <- gets location
    modify (\s -> s {location = pos})
    result <- action
    modify (\s -> s {location = oldPos})
    return result

  currentLoc = gets location

instance MonadCodegen GlobalCodegen where
  emitString = globalEmitString
  useStdCall = globalUseStdCall

execGlobalCodegen :: CodegenOptions -> GlobalCodegen a -> Either (Located CodegenError) AST.Module
execGlobalCodegen opts m =
  fmap currentModule $ runExcept $ execStateT (runGlobalCodegen m') s
 where
  m' = m >> emitCompilerInfo
  md = emptyModule opts
  s = CodegenState md Tab.empty 0 Set.empty 0 (initialPos $ sourceFileName opts)

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

-- | Convert subprogram prameter type to LLVM type
--
-- Currently all parameters are passed by value
--
paramTypeToLLVM :: Argument -> Type
paramTypeToLLVM (argty, _) = typeToLLVM argty

outArgTypes :: [Argument] -> [Type]
outArgTypes = map paramTypeToLLVM . filter (argReturned . argumentHandling)

filterInArgs :: [Argument] -> [Argument]
filterInArgs = filter (argInitialized . argumentHandling)

functionReturnType :: I.Type -> [Argument] -> Type
functionReturnType retty argtys =
  case typeToLLVM retty : outArgTypes argtys of
    [ty] -> ty
    tys -> StructureType False tys

procedureReturnType :: [Argument] -> Type
procedureReturnType argtys =
  case outArgTypes argtys of
  [] -> VoidType
  [ty] -> ty
  tys -> StructureType False tys

-- | Declares function in global symbol table
declareFun :: I.ID -> I.Type -> [Argument] -> GlobalCodegen ()
declareFun label retty argtys = addSym symt t label
 where
  symt = SymbolFunction retty argtys
  t = ptr $ FunctionType rt (map paramTypeToLLVM $ filterInArgs argtys) False
  rt = functionReturnType retty argtys

-- | Declares procedure in global symbol table
declareProc :: I.ID -> [Argument] -> GlobalCodegen ()
declareProc label argtys = addSym symt t label
 where
  symt = SymbolProcedure argtys
  t = ptr $ FunctionType (procedureReturnType argtys) (map paramTypeToLLVM $ filterInArgs argtys) False

-- | Adds global subprogram definition
defineSub :: I.ID -> Maybe I.Type -> [(I.Type, I.Mode, Located I.ID)] -> [BasicBlock] -> GlobalCodegen ()
defineSub label retty argtys body = addDefn def
 where
  argtys' = map (\(ty, mode, _id) -> (ty, mode)) argtys
  rt = case retty of
    Nothing -> procedureReturnType argtys'
    Just ty -> functionReturnType ty argtys'

  def = GlobalDefinition $
          functionDefaults { name = (mkName . getID) label
                           , parameters = ([Parameter (paramTypeToLLVM (ty, mode)) ((mkName . getID . unLoc)  nm) []
                                                   | (ty, mode, nm) <- argtys, mode /= I.ModeOut], False)
                           , returnType = rt
                           , basicBlocks = body
                           }

-- | Add global variable definition
--
-- Also adds this variable to symbol table
defineVar :: I.Type -> IsConstant -> I.ID -> GlobalCodegen ()
defineVar ty isConst label = addSym (SymbolVariable ty isConst) (ptr t) label >> addDefn def
 where
  n = mkName $ getID label
  t = typeToLLVM ty
  def = GlobalDefinition $ globalVariableDefaults { name = n, type' = t, initializer = Just $ Undef t }

newStringName :: GlobalCodegen Name
newStringName = do
  n <- gets nextStringNum
  modify $ \s -> s { nextStringNum = n + 1 }
  return $ mkName $ ".str." <> showt n

globalEmitString :: T.Text -> GlobalCodegen Operand
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
  content = TE.encodeUtf8 s
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
                       , parameters = ([Parameter ty argName [] | (ty, argName) <- args], False)
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

metadata :: MDNode -> GlobalCodegen MetadataNodeID
metadata node = do
  nd <- newMetadataNodeID
  addDefn $ MetadataNodeDefinition nd node
  return nd

emitCompilerInfo :: GlobalCodegen ()
emitCompilerInfo = do
  nd <- metadata $ MDTuple [Just $ MDString $ fromString $ "IMP version " ++ showVersion version]
  namedMetadata "llvm.ident" [nd]
