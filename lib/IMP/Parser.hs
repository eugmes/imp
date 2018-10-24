{-# LANGUAGE OverloadedStrings #-}

module IMP.Parser
  ( Parser
  , parser
  -- Other exports for testing
  , stringLiteral
  ) where

import IMP.AST
import IMP.SourceLoc
import IMP.Parser.Error
import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Functor.Syntax
import Data.List.NonEmpty (NonEmpty(..))

type Parser = Parsec CustomError T.Text

comment :: Parser ()
comment = L.skipLineComment "--"

sc :: Parser ()
sc = L.space space1 comment empty

located :: Parser a -> Parser (Located a)
located m = Located <$> getPosition <*> m

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc

lexeme :: Parser a -> Parser (Located a)
lexeme = located . lexeme'

symbol' :: T.Text -> Parser ()
symbol' = void . L.symbol sc

symbol :: T.Text -> Parser (Located ())
symbol = located . symbol'

parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")

decimal :: Integral a => Parser a
decimal = lexeme' L.decimal

stringLiteral :: Parser (Located String)
stringLiteral = lexeme stringLiteral'
 where
  stringLiteral' = char '"' *> many stringElement <* char '"'
  stringElement = try (char '"' *> char '"')
              <|> satisfy (\c -> isPrint c && c /= '"')

rword :: T.Text -> Parser ()
rword w = void $ lexeme' (string' w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "and", "begin", "boolean", "break", "call", "else", "elsif", "end"
      , "false", "function", "halt", "if", "input", "integer"
      , "is", "loop", "newline", "not", "null", "or", "output"
      , "procedure", "return", "then", "true", "var", "while"
      ]

idName :: Parser (Located String)
idName = do
  tok <- lookAhead p
  check tok
  lexeme p
 where
  p = (:) <$> letterChar <*> many alphaNumChar
  check :: String -> Parser ()
  check x = when (map toLower x `elem` rws) $
              customFailure $ RWordAsIdentifier x

comma, colon, semicolon, equals :: Parser ()
comma = symbol' ","
colon = symbol' ":"
semicolon = symbol' ";"
equals = symbol' "="

identifier :: Parser (Located ID)
identifier = ID . map toLower <$$> idName <?> "identifier"

typeName :: Parser (Located Type)
typeName  = located (IntegerType <$ rword "integer")
        <|> located (BooleanType <$ rword "boolean")

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

varDecs :: Parser [Located VarDec]
varDecs = located varDec `endBy` semicolon

subroutine :: Parser Subroutine
subroutine = procedure <|> function

subroutines :: Parser [Located Subroutine]
subroutines = located subroutine `endBy` semicolon

checkSubName :: Located ID -> Parser ()
checkSubName name = do
  endName <- unLoc <$> lookAhead identifier
  when (unLoc name /= endName) $
    customFailure $ EndMismatch name endName
  void identifier

procedure :: Parser Subroutine
procedure = do
  rword "procedure"
  name <- identifier
  params <- parens paramLists
  rword "is"
  vars <- varDecs
  body <- procBody
  checkSubName name
  return $ Procedure name params vars body

function :: Parser Subroutine
function = do
  rword "function"
  name <- identifier
  params <- parens paramLists
  rword "return"
  returnType <- typeName
  rword "is"
  vars <- varDecs
  body <- procBody
  checkSubName name
  return $ Function name params returnType vars body

procBody :: Parser Statements
procBody = between (rword "begin") (rword "end") statements

paramList :: Parser ParamList
paramList = ParamList <$> (identifier `sepBy1` comma) <* colon <*> typeName

paramLists :: Parser [Located ParamList]
paramLists = located paramList `sepBy` semicolon

statement :: Parser Statement
statement = ifStatement
        <|> WhileStatement <$> (rword "while" *> located expression <* rword "loop") <*> statements <* rword "end" <* rword "loop"
        <|> CallStatement <$> (rword "call" *> identifier) <*> parens expressions
        <|> InputStatement <$> (rword "input" *> parens identifier)
        <|> OutputStatement <$> (rword "output" *> parens expressionOrString)
        <|> NullStatement <$ rword "null"
        <|> BreakStatement <$ rword "break"
        <|> returnStatement
        <|> HaltStatement <$ rword "halt"
        <|> NewlineStatement <$ rword "newline"
        <|> AssignStatement <$> identifier <* equals <*> located expression

ifStatement :: Parser Statement
ifStatement = do
  rword "if"
  cond <- located expression
  rword "then"
  stmts <- statements
  elsifs <- many elsifPart
  elseStmts <- elsePart
  rword "end"
  rword "if"
  return $ IfStatement ((cond, stmts) :| elsifs) elseStmts

elsifPart :: Parser ConditionWithStatements
elsifPart = (,) <$> (rword "elsif" *> located expression <* rword "then") <*> statements

elsePart :: Parser Statements
elsePart = rword "else" *> statements
       <|> pure []

returnStatement :: Parser Statement
returnStatement = rword "return" *> (ReturnValStatement <$> located expression <|> pure ReturnStatement)

statements :: Parser Statements
statements = located statement `endBy` semicolon

expressionOrString :: Parser ExpressionOrString
expressionOrString = Str <$> stringLiteral
                 <|> Exp <$> located expression

expression :: Parser Expression
expression = try (BinOpExp <$> located simpleExpression <*> relOp <*> located simpleExpression)
         <|> simpleExpression

expressions :: Parser [Located Expression]
expressions = located expression `sepBy` comma

simpleExpression :: Parser Expression
simpleExpression = try (BinOpExp <$> located term <*> addOp <*> located term)
               <|> term

term :: Parser Expression
term = try (BinOpExp <$> located factor <*> mulOp <*> located factor)
   <|> factor

factor :: Parser Expression
factor = UnOpExp <$> unaryOp <*> located factor
     <|> NumberExpression <$> number
     <|> BoolExpression <$> bool
     <|> parens expression
     <|> try (CallExpression <$> identifier <*> parens expressions)
     <|> IdExpression <$> identifier
