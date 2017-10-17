module IMP.Test.Tests (tests) where

import Test.Tasty

import qualified IMP.Test.Parser as Parser

tests :: TestTree
tests = testGroup "IMP"
  [ Parser.tests
  ]
