module IMP.Parser.Error
    ( CustomError(..)
    ) where

import IMP.AST
import IMP.SourceLoc
import Text.Megaparsec.Error
import Text.Printf

data CustomError = EndMismatch (Located ID) ID
                 | RWordAsIdentifier String
                 deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  -- TODO show location of subroutine head
  showErrorComponent (EndMismatch name endName) =
    printf "\"%s\" expected but \"%s\" found." (getID $ unLoc name)
                                                   (getID endName)
  showErrorComponent (RWordAsIdentifier name) =
    printf "Reserved word \"%s\" cannot be used as identifier." name
