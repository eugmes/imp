module CompilerTests (tests) where

import Test.Tasty
import Test.Tasty.IMP

tests :: TestTree
tests = testSubDir "Example Files" "test/examples"
  [ testCase "simple"
  , testCase "hello"
  , testCase "global"
  , testCase "fibi"
  , testCase "fibi_no_rec"
  , testCase "sum"
  , testCase "gcd"
  -- TODO add integer_lit_overflow
  ]
