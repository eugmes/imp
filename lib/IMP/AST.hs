module IMP.AST where

data Program = Program [VarDec] [Subroutine] deriving Show

data VarDec = VarDec [ID] Type deriving Show

newtype ID = ID String deriving Show

data Type = IntegerType | BooleanType deriving (Show, Eq)

newtype Number = Number Integer deriving Show

data Subroutine = Procedure ID [ParamList] [VarDec] [Statement]
                | Function ID [ParamList] Type [VarDec] [Statement]
                deriving Show

data ParamList = ParamList [ID] Type deriving Show

data Statement = IfStatement Expression [Statement] [Statement]
               | WhileStatement Expression [Statement]
               | AssignStatement ID Expression
               | CallStatement ID [Expression]
               | InputStatement ID
               | OutputStatement ExpressionOrString
               | NullStatement
               | BreakStatement
               | ReturnStatement
               | ReturnValStatement Expression
               | HaltStatement
               | NewlineStatement
               deriving Show

data ExpressionOrString = Exp Expression
                        | Str String deriving Show

data Expression = UnOpExp UnaryOp Expression
                | BinOpExp Expression BinaryOp Expression
                | NumberExpression Number
                | BoolExpression Bool
                | IdExpression ID
                | CallExpression ID [Expression]
                deriving Show

data UnaryOp = OpNot | OpNeg deriving (Show, Eq)

data BinaryOp = OpEQ | OpLT | OpLE | OpGT | OpGE | OpNE
              | OpMul | OpDiv | OpMod | OpAnd
              | OpAdd | OpSub | OpOr deriving (Show, Eq)
