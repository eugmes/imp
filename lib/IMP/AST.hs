module IMP.AST where

import IMP.SourceLoc

data Program = Program [Located VarDec] [Located Subroutine] deriving Show

data VarDec = VarDec [Located ID] (Located Type) deriving Show

newtype ID = ID {getID :: String} deriving (Show, Eq, Ord)

data Type = IntegerType | BooleanType deriving (Show, Eq, Ord)

newtype Number = Number Integer deriving (Show, Eq, Ord)

data Subroutine = Procedure (Located ID) [Located ParamList] [Located VarDec] [Located Statement]
                | Function (Located ID) [Located ParamList] (Located Type) [Located VarDec] [Located Statement]
                deriving Show

data ParamList = ParamList [Located ID] (Located Type) deriving Show

data Statement = IfStatement (Located Expression) [Located Statement] [Located Statement]
               | WhileStatement (Located Expression) [Located Statement]
               | AssignStatement (Located ID) (Located Expression)
               | CallStatement (Located ID) [Located Expression]
               | InputStatement (Located ID)
               | OutputStatement ExpressionOrString
               | NullStatement
               | BreakStatement
               | ReturnStatement
               | ReturnValStatement (Located Expression)
               | HaltStatement
               | NewlineStatement
               deriving Show

data ExpressionOrString = Exp (Located Expression)
                        | Str (Located String) deriving Show

data Expression = UnOpExp UnaryOp (Located Expression)
                | BinOpExp (Located Expression) BinaryOp (Located Expression)
                | NumberExpression Number
                | BoolExpression Bool
                | IdExpression (Located ID)
                | CallExpression (Located ID) [Located Expression]
                deriving Show

data UnaryOp = OpNot | OpNeg deriving (Show, Eq, Ord)

data BinaryOp = OpEQ | OpLT | OpLE | OpGT | OpGE | OpNE
              | OpMul | OpDiv | OpMod | OpAnd
              | OpAdd | OpSub | OpOr deriving (Show, Eq, Ord)
