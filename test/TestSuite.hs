module Main (main) where

import Test.Tasty
import qualified IMP.Test.Tests as IMP
import qualified CompilerTests

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    IMP.tests,
    CompilerTests.tests
  ]
