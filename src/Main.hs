{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import IMP.Parser
import IMP.Codegen.Error
import IMP.Emit
import IMP.AST (Program)
import Text.Pretty.Simple (pShow, pShowNoColor)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Text.Megaparsec as P
import LLVM
import LLVM.Context
import LLVM.PassManager
import LLVM.Analysis
import LLVM.Exception
import LLVM.Target
import qualified LLVM.Relocation as Reloc
import qualified LLVM.Target.Options as TO
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified Data.ByteString.Char8 as C
import Options.Applicative as Opt
import Data.Semigroup ((<>))
import System.Exit
import Control.Exception
import Control.Monad
import Data.String
import qualified Data.Map as Map

data UseColor = NoColor | UseColor deriving Show
data Stage = ParseStage | CompileStage | TargetAsmStage deriving (Show, Ord, Eq, Enum, Bounded)

data Options = Options
  { inputFile :: FilePath
  , useColor :: UseColor
  , lastStage :: Stage
  , outputFile :: Maybe FilePath
  , optimizationLevel :: Maybe Word
  , triple :: Maybe String
  } deriving Show

options :: Opt.Parser Options
options = Options
  <$> strArgument (metavar "FILE" <> help "Source file name")
  <*> flag NoColor UseColor (short 'C' <> long "color" <> help "Enable color output")
  <*> ( flag' ParseStage (long "parse-only" <> help "Stop after parsing and dump the parse tree")
    <|> flag' TargetAsmStage (short 'S' <> help "Emit target assembly")
    <|> pure CompileStage )
  <*> optional (option str (short 'o' <> metavar "FILE" <> help "Redirect output to FILE"))
  <*> optional (option auto (short 'O' <> metavar "LEVEL" <> help "Set optimization level"))
  <*> optional (option str (long "triple" <> metavar "TRIPLE" <> help "Target triple for code generation"))

withTargetFromOptions :: Options -> (TargetMachine -> IO a) -> IO a
withTargetFromOptions o =
  case triple o of
    Nothing -> withHostTargetMachine
    Just t -> withNamedTarget t
 where
  withNamedTarget t f = do
    initializeAllTargets
    (target, tname) <- lookupTarget Nothing (fromString t)
    let cpu = ""
        features = Map.empty
    withTargetOptions $ \options ->
      withTargetMachine target tname cpu features options Reloc.Default CodeModel.Default CodeGenOpt.Default f

genCode :: Options -> Text -> FilePath -> Program -> IO ()
genCode o text name pgm =
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
            withPassManager passes $ \pm -> do
              void $ runPassManager pm m
              llstr <- case lastStage o of
                         CompileStage -> moduleLLVMAssembly m
                         TargetAsmStage -> moduleTargetAssembly target m
                         _ -> error "Unexpected stage"
              outputAssembly llstr
 where
  outputAssembly = maybe C.putStr C.writeFile $ outputFile o
  passes = defaultCuratedPassSetSpec { optLevel = optimizationLevel o }

outputTree :: Options -> Program -> IO ()
outputTree o tree = writeOutput $ showTree tree <> "\n"
 where
  writeOutput = maybe TLIO.putStr TLIO.writeFile $ outputFile o
  showTree = case useColor o of
             NoColor -> pShowNoColor
             UseColor -> pShow

run :: Options -> IO ()
run o = do
  let fileName = inputFile o
  text <- TIO.readFile fileName
  case P.runParser parser fileName text of
    Left e -> do
      putStr $ P.parseErrorPretty' text e
      exitFailure
    Right tree ->
      case lastStage o of
        ParseStage -> outputTree o tree
        _ -> genCode o text fileName tree `catch` \(VerifyException msg) -> do
          putStrLn "Verification exception:"
          putStrLn msg
          exitFailure

main :: IO ()
main = execParser opts >>= run
 where
  opts = info (options <**> helper)
       ( progDesc "Compiles IMP programs" )
