{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.IMP
  ( AcceptTests(..)
  , TestsDirectory(..)
  , testCase
  , testSubDir
  ) where

import Data.Typeable (Typeable)
import Data.Proxy
import Test.Tasty
import Test.Tasty.Providers
import Test.Tasty.Options
import System.Exit
import System.IO.Temp
import System.FilePath
import System.Process.ListLike
import Text.Printf
import Data.String.Builder
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception
import System.IO.Error (isDoesNotExistError)
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.State

newtype ImpTest = ImpTest FilePath
  deriving Typeable

newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord, Typeable)

instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeReadBool
  optionName = pure "imp-accept"
  optionHelp = pure "Accept current test outputs for IMP tests"
  optionCLParser = flagCLParser Nothing (AcceptTests True)

newtype TestsDirectory = TestsDirectory String
  deriving (Eq, Ord, Typeable)

instance IsOption TestsDirectory where
  defaultValue = TestsDirectory ""
  parseValue = Just . TestsDirectory
  optionName = pure "imp-tests-dir"
  optionHelp = pure "Root directory for IMP test files"

readFileIfExists :: FilePath -> IO (Maybe T.Text)
readFileIfExists name = do
  mbBs <- try $ TIO.readFile name
  case mbBs of
    Left e | isDoesNotExistError e -> pure Nothing
           | otherwise -> throwIO e
    Right bs -> pure $ Just bs

instance IsTest ImpTest where
  testOptions =
    pure [ Option (Proxy :: Proxy AcceptTests)
         , Option (Proxy :: Proxy TestsDirectory)
         ]

  run opts (ImpTest name) _ =
    withSystemTempDirectory name $ \tmpDir -> do
      let outputFileName = tmpDir </> name
          args = ["-o", outputFileName, sourceFileName]
      (r, stdOut, stdErr) <- readCreateProcessWithExitCode (proc "impc" args) ""

      case r of
        ExitSuccess ->
          runExecutable outputFileName
        ExitFailure code -> pure $ testFailed $ build $ do
          literal $ printf "Compilation failed with exit code %d" code

          unless (null stdOut) $ do
            "\nCompiler output:\n"
            literal stdOut

          unless (null stdErr) $ do
            "\nCompiler error output:\n"
            literal stdErr
   where
    AcceptTests accept = lookupOption opts
    TestsDirectory dir = lookupOption opts
    sourceBaseName = dir </> name
    sourceFileName = sourceBaseName <.> "imp"
    stdInFileName = sourceBaseName <.> "stdin"
    stdOutFileName = sourceBaseName <.> "stdout"
    stdErrFileName = sourceBaseName <.> "stderr"

    runExecutable :: FilePath -> IO Result
    runExecutable exeFileName = do
      input <- readFileIfExists stdInFileName
      expStdOut <- readFileIfExists stdOutFileName
      expStdErr <- readFileIfExists stdErrFileName
      (r, stdOut, stdErr) <- readCreateProcessWithExitCode (proc exeFileName []) (fromMaybe "" input)

      case r of
        ExitSuccess -> do
          (strs, ok) <- flip runStateT True $ execWriterT $ do
            compareOutput "stdout" stdOutFileName expStdOut stdOut
            compareOutput "stderr" stdErrFileName expStdErr stdErr
          let func = if ok then testPassed else testFailed
          pure $ func $ unlines strs
        ExitFailure code -> pure $ testFailed $ printf "Execution failed with exit code %d" code

    compareOutput :: String -> FilePath -> Maybe T.Text -> T.Text -> (WriterT [String] (StateT Bool IO)) ()
    compareOutput _ _ Nothing _ = pure ()

    compareOutput label fileName (Just expected) actual =
      if expected /= actual
        then
          if accept
            then do
              liftIO $ TIO.writeFile fileName actual
              tell [ printf "Accepted new verison (%s)" label ]
            else do
              tell [ printf "Output differs (%s)" label ]
              put False
        else pure ()

testCase :: TestName -> TestTree
testCase name = singleTest name $ ImpTest name

testSubDir :: TestName -> String -> [TestTree] -> TestTree
testSubDir name subDir = adjustOption (\(TestsDirectory dir) -> TestsDirectory $ dir </> subDir) . testGroup name
