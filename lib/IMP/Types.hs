module IMP.Types where

import IMP.AST

data IsConstant = NotConstant
                | IsConstant
                deriving Show

type Argument = (Type, Mode);

data SymbolType = SymbolVariable Type IsConstant
                | SymbolProcedure [Argument]
                | SymbolFunction Type [Argument]
                deriving Show

data ArgumentHandling = ArgumentHandling
                      { argInitialized :: !Bool
                      , argReturned :: !Bool
                      , argIsConstant :: !IsConstant
                      }

argumentHandling :: Argument -> ArgumentHandling
argumentHandling (_, ModeIn) = ArgumentHandling True False IsConstant
argumentHandling (_, ModeOut) = ArgumentHandling False True NotConstant
argumentHandling (_, ModeInOut) = ArgumentHandling True True NotConstant
