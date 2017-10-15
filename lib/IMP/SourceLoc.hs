{-# LANGUAGE DeriveFunctor #-}
-- | Utilities for working with source locations.
module IMP.SourceLoc ( Located(..)
                     , WithLoc(..)
                     , SourcePos(..)
                     , initialPos
                     , withLoc
                     ) where

import Text.Megaparsec.Pos

-- | Type @'Located' a@ represents values of type @a@ with source location
-- attached to them.
data Located a = Located
               { getLoc :: SourcePos
               , unLoc :: a
               } deriving (Eq, Ord, Show, Functor)

-- | @'WithLoc' m@ represents computations with associated source location.
--
-- Instances of 'WithLoc' should satisfy the following laws:
--
-- * @'withNewLoc' p 'currentLoc' == 'pure' p@
--
class Applicative f => WithLoc f where
  -- | The expression (@'withNewLoc' p f@) returns computation @f@ with location
  -- @p@ set as context.
  withNewLoc :: SourcePos -> f a -> f a
  -- | Pure value containing current location.
  currentLoc :: f SourcePos

-- | The expression (@'withLoc' f x@) passes from @x@ to the
-- function @f@ and returns computation with location from @x@.
withLoc :: WithLoc f => (t -> f a) -> Located t -> f a
withLoc f (Located p a) = withNewLoc p (f a)
