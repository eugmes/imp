{-# LANGUAGE OverloadedStrings #-}
module IMP.Test.Parser (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import IMP.Parser
import IMP.SourceLoc

import Data.Text (Text)
import Text.Megaparsec

validParseTest :: (Eq b, Show b) => Parser a -> (a -> b) -> Text -> b -> TestTree
validParseTest p f input expected = testCase name $ Just expected @=? actual
 where
  name = "for " ++ show expected
  actual = f <$> parseMaybe p input

stringLiteralTests :: [(Text, String)]
stringLiteralTests =
  [ ("\"Message of the day:\"", "Message of the day:")
  , ("\"\"", "")
  , ("\" \"", " ")
  , ("\"A\"", "A")
  , ("\"\"\"\"", "\"")
  , ("\"Characters such as $, %, and } are allowed in string literals\"",
     "Characters such as $, %, and } are allowed in string literals")
  , ("\"Archimedes said \"\"Εύρηκα\"\"\"",
     "Archimedes said \"Εύρηκα\"")
  , ("\"Volume of cylinder (πr²h) = \"",
     "Volume of cylinder (πr²h) = ")
  ]

stringTests :: [TestTree]
stringTests = map f stringLiteralTests
 where
  f (i, r) = validParseTest stringLiteral unLoc i r

tests :: TestTree
tests = testGroup "Parser"
  [ testGroup "stringLiteral" stringTests
  ]
