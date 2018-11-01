{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import IMP.Parser
import IMP.Codegen.Error
import IMP.Emit
import IMP.AST (Program)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P
import LLVM
import LLVM.Context
import LLVM.PassManager as PM
import LLVM.Analysis
import LLVM.Exception
import LLVM.Target
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified Data.ByteString.Char8 as C
import LLVM.CommandLine
import Options.Applicative as Opt
import Data.Semigroup ((<>))
import System.Exit
import Control.Exception
import Control.Monad
import Data.String
import qualified Data.Map as Map
import System.Environment (getProgName, lookupEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath
import System.Process
import Data.Maybe
import Paths_imp (getDataFileName)

getStdLibrarySource :: IO FilePath
getStdLibrarySource = getDataFileName $ "stdlib" </> "impstd.c"

getDefaultCCompiler :: IO FilePath
getDefaultCCompiler = fromMaybe "clang" <$> lookupEnv "CC"

data Stage = ParseStage
           | AssemblyStage
           | ObjectStage
           | LinkStage
           deriving (Show, Ord, Eq, Enum, Bounded)

data OutputKind = NativeOutput
                | LLVMOutput
                deriving Show

data Options = Options
  { inputFile :: FilePath
  , lastStage :: Stage
  , outputKind :: OutputKind
  , outputFile :: Maybe FilePath
  , optimizationLevel :: Maybe Word
  , triple :: Maybe String
  , cpu :: Maybe String
  , llvmOptions :: [String]
  , cc :: Maybe FilePath
  } deriving Show

getDefaultOutputExt :: Options -> String
getDefaultOutputExt o =
  case (lastStage o, outputKind o) of
  (ParseStage, _) -> "tree"
  (AssemblyStage, NativeOutput) -> "s"
  (AssemblyStage, LLVMOutput) -> "ll"
  (ObjectStage, NativeOutput) -> "o"
  (ObjectStage, LLVMOutput) -> "bc"
  (LinkStage, _) -> ""

makeOutputFileName :: Options -> Maybe FilePath
makeOutputFileName o =
  case outputFile o of
  Just "-" -> Nothing
  Just name -> Just name
  Nothing -> Just $ takeBaseName (inputFile o) <.> getDefaultOutputExt o

options :: Opt.Parser Options
options = Options
  <$> strArgument (metavar "FILE" <> help "Source file name")
  <*> ( flag' ParseStage (long "parse-only" <> help "Stop after parsing and dump the parse tree")
    <|> flag' AssemblyStage (short 'S' <> help "Emit assembly")
    <|> flag' ObjectStage (short 'c' <> help "Emit object code/bitcode")
    <|> pure LinkStage )
  <*> ( flag' LLVMOutput (long "emit-llvm" <> help "Emit LLVM assembly code/bitcode")
    <|> pure NativeOutput )
  <*> optional (option str (short 'o' <> metavar "FILE" <> help "Redirect output to FILE"))
  <*> optional (option auto (short 'O' <> metavar "LEVEL" <> help "Set optimization level"))
  <*> optional (option str (long "triple" <> metavar "TRIPLE" <> help "Target triple for code generation"))
  <*> optional (option str (long "cpu" <> metavar "CPU" <> help "Target a specific CPU type"))
  <*> many (option str (long "llvm" <> metavar "OPTION" <> help "Additional options to pass to LLVM"))
  <*> optional (option str (long "cc" <> metavar "PATH" <> help "Use specified C compiler for linking"))

withTargetFromOptions :: Options -> (TargetMachine -> IO a) -> IO a
withTargetFromOptions o f = do
  initializeAllTargets
  triple <- case triple o of
    Nothing -> getDefaultTargetTriple
    Just t -> return $ fromString t
  (target, tname) <- lookupTarget Nothing triple
  let cpuName = maybe "" fromString $ cpu o
      features = Map.empty
  withTargetOptions $ \options ->
    withTargetMachine target tname cpuName features options Reloc.Default CodeModel.Default CodeGenOpt.Default f

setLLVMCommandLineOptions :: [String] -> IO ()
setLLVMCommandLineOptions [] = return ()
setLLVMCommandLineOptions opts = do
  prog <- getProgName
  let args = map fromString $ prog : opts
  parseCommandLineOptions args Nothing

genCode :: Options -> Text -> FilePath -> Program -> IO ()
genCode o text name pgm = do
  setLLVMCommandLineOptions $ llvmOptions o
  withContext $ \context ->
    withTargetFromOptions o $ \target -> do
      dataLayout <- getTargetMachineDataLayout target
      targetTriple <- getTargetMachineTriple target
      let opts = CodegenOptions name dataLayout targetTriple

      case compileProgram opts pgm of
        Left err -> do
          putStrLn $ locatedErrorPretty text err
          exitFailure
        Right ast ->
          withModuleFromAST context ast $ \m -> do
            verify m
            let passes = defaultCuratedPassSetSpec
                       { PM.optLevel = optimizationLevel o
                       , PM.dataLayout = Just dataLayout
                       , PM.targetMachine = Just target
                       }
            withPassManager passes $ \pm -> do
              void $ runPassManager pm m
              llstr <- case (lastStage o, outputKind o) of
                (AssemblyStage, NativeOutput) -> moduleTargetAssembly target m
                (AssemblyStage, LLVMOutput) -> moduleLLVMAssembly m
                (ObjectStage, NativeOutput) -> moduleObject target m
                (ObjectStage, LLVMOutput) -> moduleBitcode m
                _ -> error "Unexpected stage"
              outputAssembly llstr
 where
  outputAssembly = maybe C.putStr C.writeFile $ makeOutputFileName o

outputTree :: Options -> Program -> IO ()
outputTree o tree = writeOutput $ show tree <> "\n"
 where
  writeOutput = maybe putStr writeFile $ makeOutputFileName o

linkProgram :: Options -> FilePath -> FilePath -> IO ()
linkProgram o objectFileName outputFileName = do
  stdLibFile <- getStdLibrarySource
  cc <- maybe getDefaultCCompiler pure $ cc o
  let optOption = "-O" <> maybe "2" show (optimizationLevel o)
      compilerArgs = [ "-o", outputFileName
                     , objectFileName, stdLibFile
                     , optOption
                     , "-nopie" ] -- Needed when using GCC in some environments
  callProcess cc compilerArgs

run :: Options -> IO ()
run o = do
  let fileName = inputFile o
  text <- TIO.readFile fileName
  case P.runParser parser fileName text of
    Left e -> do
      putStr $ P.parseErrorPretty' text e
      exitFailure
    Right tree ->
      let
        runGenCode opts = genCode opts text fileName tree `catch` \(VerifyException msg) -> do
           putStrLn "Verification exception:"
           putStrLn msg
           exitFailure
      in
      case lastStage o of
        ParseStage -> outputTree o tree
        LinkStage ->
          case makeOutputFileName o of
            Nothing -> do
              putStrLn "Refusing to produce executable on standard output."
              exitFailure
            Just outputFileName ->
              withSystemTempDirectory "imp" $ \dir -> do
                let tmpExt = getDefaultOutputExt $ o { lastStage = ObjectStage }
                    tmpFileName = dir </> takeBaseName fileName <.> tmpExt
                    opts = o { lastStage = ObjectStage, outputFile = Just tmpFileName }
                runGenCode opts
                linkProgram o tmpFileName outputFileName
        _ -> runGenCode o

main :: IO ()
main = execParser opts >>= run
 where
  opts = info (options <**> helper)
       ( progDesc "Compiles IMP programs" )
