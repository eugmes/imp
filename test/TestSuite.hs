module Main (main) where

import Test.Tasty
import qualified IMP.Test.Tests as IMP

main :: IO ()
main = defaultMain IMP.tests
