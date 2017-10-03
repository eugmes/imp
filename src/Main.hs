{-# LANGUAGE OverloadedStrings #-}

module Main where

import IMP.Parser
import IMP.Codegen
import IMP.Emit
import IMP.AST (Program)
import Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Text.Megaparsec as P
import LLVM
import LLVM.Context
import LLVM.PassManager
import LLVM.Analysis
import LLVM.Exception
import LLVM.Target
import qualified Data.ByteString.Char8 as C
import Options.Applicative as Opt
import Data.Semigroup ((<>))
import System.Exit
import Control.Exception
import Control.Monad

data UseColor = NoColor | UseColor deriving Show
data Stage = ParseStage | CompileStage deriving (Show, Ord, Eq, Enum, Bounded)

data Options = Options
  { inputFile :: FilePath
  , useColor :: UseColor
  , lastStage :: Stage
  , outputFile :: Maybe FilePath
  , optimizationLevel :: Maybe Word
  } deriving Show

options :: Opt.Parser Options
options = Options
  <$> strArgument (metavar "FILE" <> help "Source file name")
  <*> flag NoColor UseColor (short 'C' <> long "color" <> help "Enable color output")
  <*> flag CompileStage ParseStage (long "parse-only" <> help "Stop after parsing and dump the parse tree")
  <*> optional (option str (short 'o' <> metavar "FILE" <> help "Redirect output to FILE"))
  <*> optional (option auto (short 'O' <> metavar "LEVEL" <> help "Set optimization level"))

genCode :: Options -> FilePath -> Program -> IO ()
genCode o name pgm = do
  initializeNativeTarget
  withContext $ \context ->
    withHostTargetMachine $ \target -> do
      dataLayout <- getTargetMachineDataLayout target
      targetTriple <- getTargetMachineTriple target
      let mod = emptyModule name name dataLayout targetTriple
          ast = runLLVM mod $ codegenProgram pgm
      withModuleFromAST context ast $ \m -> do
        verify m
        withPassManager passes $ \pm -> do
          void $ runPassManager pm m
          llstr <- moduleLLVMAssembly m
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
        _ -> genCode o fileName tree `catch` \(VerifyException msg) -> do
               putStrLn "Verification exception:"
               putStrLn msg
               exitFailure

main :: IO ()
main = execParser opts >>= run
 where
  opts = info (options <**> helper)
       ( progDesc "Compiles IMP programs" )
