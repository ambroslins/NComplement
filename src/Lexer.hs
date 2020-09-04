module Lexer
  ( Parser,
    lexeme,
    symbol,
    identifier,
    reserved,
    natural,
    integer,
    real,
    parens,
    braces,
    semicolon,
    comma,
    Lex.charLiteral,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lex.space
    (void $ some (oneOf [' ', '\t']))
    (Lex.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

symbol :: Text -> Parser Text
symbol = Lex.symbol sc

identifier :: Parser Text
identifier = lexeme $ do
  x <- lowerChar <|> oneOf ['_', '\'']
  xs <- many $ alphaNumChar <|> char '_'
  pure $ Text.pack (x : xs)

reserved :: Text -> Parser ()
reserved x = lexeme $ string x >> notFollowedBy (alphaNumChar <|> char '_')

natural :: (Integral a, Num a) => Parser a
natural = lexeme Lex.decimal

integer :: (Integral a, Num a) => Parser a
integer = lexeme $ Lex.signed sc natural

real :: Parser Double
real = lexeme $ Lex.signed sc Lex.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{" <* nl) (nl *> symbol "}")
  where
    nl = lexeme $ optional eol

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","
