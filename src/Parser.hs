module Parser where

import AST
import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
import qualified Data.Set as Set
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
statement = ifStatement <|> assignment
  where
    ifStatement = do
      reserved "if"
      cond <- expression
      sThen <- braces statements
      sElse <- option [] $ reserved "else" *> braces statements
      pure $ If cond sThen sElse
    assignment = do
      var <- identifier
      reserved "="
      e <- expression
      pure $ Assignment var e

expression :: Parser Expression
expression =
  makeExprParser term table <?> "expression"

term :: Parser Expression
term = parens expression <|> literal <|> reference <|> variabel
  where
    literal =
      Lit
        <$> ( LitBool True <$ symbol "True"
                <|> LitBool False <$ symbol "False"
                <|> LitReal <$> try real
                <|> LitInteger <$> integer
            )
    reference = char '&' >> Ref <$> identifier
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
  let i = x : xs
  when (Set.member i reservedNames) $ fail $ i ++ " is a reserved keyword"
  pure $ x : xs
  where
    reservedNames = Set.fromList ["if", "else"]

reserved :: String -> Parser ()
reserved x = lexeme $ string x >> notFollowedBy (alphaNumChar <|> char '_')

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
