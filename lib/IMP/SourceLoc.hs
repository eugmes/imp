{-# LANGUAGE DeriveFunctor #-}

module IMP.SourceLoc (Located(..)) where

import Text.Megaparsec.Pos

data Located a = Located
               { getLoc :: SourcePos
               , unLoc :: a
               } deriving (Eq, Ord, Show, Functor)
