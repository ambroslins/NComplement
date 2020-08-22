module Parser where

import AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

statements :: Parser [Statement]
statements = sepEndBy statement sep
  where
    sep = lexeme $ void (semicolon >> optional newline) <|> void newline

statement :: Parser Statement
statement = assignment <|> ifStatement
  where
    assignment = try $ do
      var <- identifier
      _ <- symbol "="
      e <- expression
      pure $ Assignment var e
    ifStatement = try $ do
      _ <- symbol "if"
      cond <- expression
      sThen <- braces statements
      sElse <- option [] $ symbol "else" *> braces statements
      pure $ If cond sThen sElse

expression :: Parser Expression
expression =
  makeExprParser term table <?> "expression"

term :: Parser Expression
term = parens expression <|> Lit <$> literal <|> variabel
  where
    literal =
      (symbol "True" *> pure (LitBool True))
        <|> (symbol "False" *> pure (LitBool False))
        <|> LitReal <$> try real
        <|> LitInteger <$> integer
    variabel = Var <$> identifier

table :: [[Operator Parser Expression]]
table =
  [ [prefix "-" Neg, prefix "+" id],
    [infixLeft "*" Mul, infixLeft "/" Div],
    [infixLeft "+" Add, infixLeft "-" Sub],
    [infixNone "=" Eq, infixNone "<" Lt, infixNone ">" Gt]
  ]
  where
    prefix name f = Prefix (f <$ symbol name)
    infixLeft name f = InfixL (f <$ symbol name)
    infixNone name f = InfixN (f <$ symbol name)

sc :: Parser ()
sc =
  Lexer.space
    (void $ some (oneOf " \t"))
    (Lexer.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

identifier :: Parser String
identifier = lexeme $ do
  x <- lowerChar <|> char '_'
  xs <- many $ alphaNumChar <|> char '_'
  pure $ x : xs

natural :: Parser Integer
natural = lexeme Lexer.decimal

integer :: Parser Integer
integer = lexeme $ Lexer.signed empty natural

real :: Parser Double
real = lexeme Lexer.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semicolon :: Parser String
semicolon = symbol ";"

comma :: Parser String
comma = symbol ","
