module IMP.AST where

import IMP.SourceLoc

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

data Program = Program [Located VarDec] [Located Subroutine] deriving Show

data VarDec = VarDec [Located ID] (Located Type) deriving Show

newtype ID = ID {getID :: T.Text} deriving (Show, Eq, Ord)

data Type = IntegerType | BooleanType | StringType deriving (Show, Eq, Ord)

newtype Number = Number Integer deriving (Show, Eq, Ord)

data Subroutine = Procedure (Located ID) [Located ParamList] [Located VarDec] Statements
                | Function (Located ID) [Located ParamList] (Located Type) [Located VarDec] Statements
                deriving Show

data ParamList = ParamList [Located ID] Mode (Located Type) deriving Show

data Mode = ModeIn
          | ModeOut
          | ModeInOut
          deriving (Eq, Ord, Show)

data Statement = IfStatement (NonEmpty ConditionWithStatements) Statements
               | WhileStatement (Located Expression) Statements
               | AssignStatement (Located ID) (Located Expression)
               | CallStatement (Located ID) [Located Expression]
               | InputStatement (Located ID)
               | OutputStatement (Located Expression)
               | NullStatement
               | BreakStatement
               | ReturnStatement
               | ReturnValStatement (Located Expression)
               | HaltStatement
               | NewlineStatement
               deriving Show

type Statements = [Located Statement]

type ConditionWithStatements = (Located Expression, Statements)

data Expression = UnOpExp UnaryOp (Located Expression)
                | BinOpExp (Located Expression) BinaryOp (Located Expression)
                | NumberExpression Number
                | BoolExpression Bool
                | IdExpression (Located ID)
                | CallExpression (Located ID) [Located Expression]
                | StringLiteralExpression (Located T.Text)
                deriving Show

data UnaryOp = OpNot | OpNeg deriving (Show, Eq, Ord)

data BinaryOp = OpEQ | OpLT | OpLE | OpGT | OpGE | OpNE
              | OpMul | OpDiv | OpMod | OpAnd
              | OpAdd | OpSub | OpOr deriving (Show, Eq, Ord)
