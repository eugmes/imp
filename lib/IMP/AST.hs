module IMP.AST where

import IMP.SourceLoc

data Program = Program [VarDec] [Subroutine] deriving Show

data VarDec = VarDec [Located ID] (Located Type) deriving Show

newtype ID = ID {getID :: String} deriving (Show, Eq, Ord)

data Type = IntegerType | BooleanType deriving (Show, Eq)

newtype Number = Number Integer deriving Show

data Subroutine = Procedure (Located ID) [ParamList] [VarDec] [Statement]
                | Function (Located ID) [ParamList] (Located Type) [VarDec] [Statement]
                deriving Show

data ParamList = ParamList [Located ID] (Located Type) deriving Show

data Statement = IfStatement Expression [Statement] [Statement]
               | WhileStatement Expression [Statement]
               | AssignStatement (Located ID) Expression
               | CallStatement (Located ID) [Expression]
               | InputStatement (Located ID)
               | OutputStatement ExpressionOrString
               | NullStatement
               | BreakStatement
               | ReturnStatement
               | ReturnValStatement Expression
               | HaltStatement
               | NewlineStatement
               deriving Show

data ExpressionOrString = Exp Expression
                        | Str (Located String) deriving Show

data Expression = UnOpExp UnaryOp Expression
                | BinOpExp Expression BinaryOp Expression
                | NumberExpression Number
                | BoolExpression Bool
                | IdExpression (Located ID)
                | CallExpression (Located ID) [Expression]
                deriving Show

data UnaryOp = OpNot | OpNeg deriving (Show, Eq)

data BinaryOp = OpEQ | OpLT | OpLE | OpGT | OpGE | OpNE
              | OpMul | OpDiv | OpMod | OpAnd
              | OpAdd | OpSub | OpOr deriving (Show, Eq)
