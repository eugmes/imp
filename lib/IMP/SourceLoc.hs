{-# LANGUAGE DeriveFunctor #-}

module IMP.SourceLoc ( Located(..)
                     , MonadLoc(..)
                     , SourcePos(..)
                     , initialPos
                     ) where

import Text.Megaparsec.Pos

data Located a = Located
               { getLoc :: SourcePos
               , unLoc :: a
               } deriving (Eq, Ord, Show, Functor)

class MonadLoc m where
  withLoc :: (a -> m b) -> Located a -> m b
  currentLoc :: m SourcePos
