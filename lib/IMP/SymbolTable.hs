module IMP.SymbolTable
    ( SymbolTable
    , empty
    , newScope
    , insert
    , lookup
    ) where

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Monoid
import Prelude hiding (lookup)

newtype SymbolTable k v = SymbolTable (NonEmpty (Map.Map k v)) deriving Show

-- | Creates an empty symbol table.
empty :: SymbolTable k v
empty = SymbolTable (Map.empty :| [])

-- | Starts new scope in symbol table.
newScope :: SymbolTable k v -> SymbolTable k v
newScope (SymbolTable l) = SymbolTable $ Map.empty <| l

-- | Insert new symbol into symbol table.
--
-- The expression (@'insert' k v symtab@) inserts a new value @v@ with key @k@
-- into the active scope if it is not already present there.
--
-- In case of success returns updated symbol table as @'Rigth'@. In case
-- of error an old value of the symbol is returned as @'Left'@.
insert :: Ord k => k -> v -> SymbolTable k v -> Either v (SymbolTable k v)
insert k v (SymbolTable (s :| ss)) =
    case Map.lookup k s of
        Nothing -> Right $ SymbolTable $ Map.insert k v s :| ss
        Just v -> Left v

-- | Lookup value in symbol table.
--
-- The expression (@'lookup' k symtab'@) looks up a value with key
-- @k@ in @symtab@ and returns it as @'Just'@ if found. Otherwise
-- returns @'Nothing'@.
lookup :: Ord k => k -> SymbolTable k v -> Maybe v
lookup k (SymbolTable ss) = getFirst $ foldMap (First . Map.lookup k) ss
