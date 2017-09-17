{-# LANGUAGE OverloadedStrings #-}

module IMP.Parser (Parser, parser) where

import IMP.AST
import Control.Applicative
import Control.Monad
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

decimal :: Integral a => Parser a
decimal = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = lexeme stringLiteral'
  where
    stringLiteral' = char '"' >> manyTill L.charLiteral (char '"')

rword :: T.Text -> Parser ()
rword w = lexeme (string' w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "var"
      , "procedure"
      , "function"
      , "return"
      , "is"
      , "begin"
      , "end"
      , "if"
      , "then"
      , "else"
      , "while"
      , "loop"
      , "call"
      , "input"
      , "output"
      , "null"
      , "break"
      , "halt"
      , "newline"
      , "not"
      , "false"
      , "true"
      , "and"
      , "or"
      , "integer"
      , "boolean"
      ]

idName :: Parser String
idName = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if map toLower x `elem` rws
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x

comma, colon, semicolon, equals :: Parser ()
comma = symbol ","
colon = symbol ":"
semicolon = symbol ";"
equals = symbol "="

identifier :: Parser ID
identifier = ID . map toLower <$> idName <?> "identifier"

typeName :: Parser Type
typeName  = IntegerType <$ rword "integer"
        <|> BooleanType <$ rword "boolean"

bool :: Parser Bool
bool = False <$ rword "false"
   <|> True <$ rword "true"

number :: Parser Number
number = Number <$> decimal

relOp :: Parser BinaryOp
relOp = OpEQ <$ symbol "=="
    <|> OpLE <$ try (symbol "<=")
    <|> OpLT <$ symbol "<"
    <|> OpGE <$ try (symbol ">=")
    <|> OpGT <$ symbol ">"
    <|> OpNE <$ symbol "#"

mulOp :: Parser BinaryOp
mulOp = OpMul <$ symbol "*"
    <|> OpDiv <$ symbol "/"
    <|> OpMod <$ symbol "%"
    <|> OpAnd <$ rword "and"

addOp :: Parser BinaryOp
addOp = OpAdd <$ symbol "+"
    <|> OpSub <$ symbol "-"
    <|> OpOr <$ rword "or"

unaryOp :: Parser UnaryOp
unaryOp = OpNot <$ rword "not"
      <|> OpNeg <$ symbol "-"

parser :: Parser Program
parser = between sc eof program

program :: Parser Program
program = Program <$> varDecs <*> subroutines

varDec :: Parser VarDec
varDec = VarDec <$> (rword "var" *> (identifier `sepBy` comma) <* colon) <*> typeName

varDecs :: Parser [VarDec]
varDecs = varDec `endBy` semicolon 

subroutine :: Parser Subroutine
subroutine = procedure <|> function

subroutines :: Parser [Subroutine]
subroutines = subroutine `endBy` semicolon

procedure :: Parser Subroutine
procedure = do 
    rword "procedure"
    name@(ID startName) <- identifier
    params <- parens paramLists
    rword "is"
    vars <- varDecs
    body <- procBody
    ID endName <- identifier
    when (startName /= endName) $
        fail $ "Procedure name mismatch after end. Expecting " ++ show startName ++ " but found " ++ show endName
    return $ Procedure name params vars body

function :: Parser Subroutine
function = do
    rword "function"
    name@(ID startName) <- identifier
    params <- parens paramLists
    rword "return"
    returnType <- typeName
    rword "is"
    vars <- varDecs
    body <- procBody
    ID endName <- identifier
    when (startName /= endName) $
        fail $ "Function name mismatch after end. Expecting " ++ show startName ++ " but found " ++ show endName
    return $ Function name params returnType vars body

procBody :: Parser [Statement]
procBody = between (rword "begin") (rword "end") statements

paramList :: Parser ParamList
paramList = ParamList <$> (identifier `sepBy1` comma) <* colon <*> typeName

paramLists :: Parser [ParamList]
paramLists = paramList `sepBy` semicolon

statement :: Parser Statement
statement = IfStatement <$> (rword "if" *> expression <* rword "then") <*> statements <*> elsePart <* rword "end" <* rword "if"
        <|> WhileStatement <$> (rword "while" *> expression <* rword "loop") <*> statements <* rword "end" <* rword "loop"
        <|> CallStatement <$> (rword "call" *> identifier) <*> parens expressions
        <|> InputStatement <$> (rword "input" *> parens identifier)
        <|> OutputStatement <$> (rword "output" *> parens expressionOrString)
        <|> NullStatement <$ rword "null"
        <|> BreakStatement <$ rword "break"
        <|> returnStatement
        <|> HaltStatement <$ rword "halt"
        <|> NewlineStatement <$ rword "newline"
        <|> AssignStatement <$> identifier <* equals <*> expression

elsePart :: Parser [Statement]
elsePart = rword "else" *> statements
       <|> pure []

returnStatement :: Parser Statement
returnStatement = rword "return" *> (ReturnValStatement <$> expression <|> pure ReturnStatement)

statements :: Parser [Statement]
statements = statement `endBy` semicolon

expressionOrString :: Parser ExpressionOrString
expressionOrString = Str <$> stringLiteral
                 <|> Exp <$> expression

expression :: Parser Expression
expression = try (BinOpExp <$> simpleExpression <*> relOp <*> simpleExpression)
         <|> simpleExpression

expressions :: Parser [Expression]
expressions = expression `sepBy` comma

simpleExpression :: Parser Expression
simpleExpression = try (BinOpExp <$> term <*> addOp <*> term)
               <|> term

term :: Parser Expression
term = try (BinOpExp <$> factor <*> mulOp <*> factor)
   <|> factor

factor :: Parser Expression
factor = UnOpExp <$> unaryOp <*> factor
     <|> NumberExpression <$> number
     <|> BoolExpression <$> bool
     <|> parens expression
     <|> try (CallExpression <$> identifier <*> parens expressions)
     <|> IdExpression <$> identifier
