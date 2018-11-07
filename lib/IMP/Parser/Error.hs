module IMP.Parser.Error
    ( CustomError(..)
    ) where

import IMP.AST
import IMP.SourceLoc
import Text.Megaparsec.Error
import Text.Printf
import qualified Data.Text as T

data CustomError = EndMismatch (Located ID) ID
                 | RWordAsIdentifier T.Text
                 | IdentifierEndsWithPunctConn T.Text
                 | IdentifierContainsTwoPunctConn T.Text
                 deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  -- TODO show location of subroutine head
  showErrorComponent (EndMismatch name endName) =
    printf "\"%s\" expected but \"%s\" found." (getID $ unLoc name)
                                                   (getID endName)
  showErrorComponent (RWordAsIdentifier name) =
    printf "Reserved word \"%s\" cannot be used as identifier." name

  showErrorComponent (IdentifierEndsWithPunctConn name) =
    printf "Identifier \"%s\" ends with a character in category punctuation_connector." name

  showErrorComponent (IdentifierContainsTwoPunctConn name) =
    printf "Identifier \"%s\" contains two consecutive characters in category punctuation_connector." name
