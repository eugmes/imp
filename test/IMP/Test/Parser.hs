{-# LANGUAGE OverloadedStrings #-}
module IMP.Test.Parser (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import IMP.Parser
import IMP.SourceLoc
import IMP.AST

import Data.Text (Text, unpack)
import Text.Megaparsec
import Data.Char
import Text.Printf

-- | Escape invalid characters in string tst cases so they lookg better in the
-- test report.
escapeString :: Text -> String
escapeString = concatMap escape . unpack
 where
  escape c
    | isPrint c = [c]
    | otherwise = printf "#{%02X}" $ fromEnum c

validParseTest :: (Eq b, Show b) => Parser a -> (a -> b) -> Text -> b -> TestTree
validParseTest p f input expected = testCase name $ Just expected @=? actual
 where
  name = "for " <> escapeString input
  actual = f <$> parseMaybe p input

invalidParseTest :: (Eq a, Show a) => Parser a -> Text -> TestTree
invalidParseTest p input = testCase name $ Nothing @=? actual
 where
  name = "for " <> escapeString input
  actual = parseMaybe p input

stringLiteralTests :: [(Text, Text)]
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

invalidStringLiteralTests :: [Text]
invalidStringLiteralTests =
  [ "\"unterminated string"
  , "\"string with embedded newline\n\""
  , "\"string with tab\tcharacter\""
  ]

stringTests :: [TestTree]
stringTests = map f stringLiteralTests ++ map f' invalidStringLiteralTests
 where
  f (i, r) = validParseTest stringLiteral unLoc i r
  f' = invalidParseTest stringLiteral

validIdentifierTests :: [(Text, Text)]
validIdentifierTests =
  [ ("Count", "count")
  , ("X", "x")
  , ("Get_Symbol", "get_symbol")
  , ("Ethelyn", "ethelyn")
  , ("Marion", "marion")
  , ("Snobol_4", "snobol_4")
  , ("X1", "x1")
  , ("Page_Count", "page_count")
  , ("Store_Next_Item", "store_next_item")
  , ("Πλάτω", "πλάτω")
  , ("Чайковский", "чайковский")
  , ("θ", "θ")
  , ("φ", "φ")
  ]

invalidIdentifierTests :: [Text]
invalidIdentifierTests = [ "123", "begin", "two__underscores", "ends_with_undercore_" ]

identifierTests :: [TestTree]
identifierTests = map f validIdentifierTests ++ map f' invalidIdentifierTests
 where
  f (i, r) = validParseTest identifier (getID . unLoc) i r
  f' = invalidParseTest identifier

tests :: TestTree
tests = testGroup "Parser"
  [ testGroup "stringLiteral" stringTests
  , testGroup "identifier" identifierTests
  ]
