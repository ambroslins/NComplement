module Parser
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
    Text,
    try,
    sepEndBy,
    many,
    choice,
    option,
    char,
    newline,
    (<|>),
    (<?>),
    NonEmpty.some,
    NonEmpty.someTill,
  )
where

import Control.Monad (void)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    (void $ some (oneOf [' ', '\t']))
    (Lexer.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

identifier :: Parser Text
identifier = lexeme $ do
  x <- lowerChar <|> char '_'
  xs <- many $ alphaNumChar <|> char '_'
  pure $ Text.pack (x : xs)

reserved :: Text -> Parser ()
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
braces = between (symbol "{" <* nl) (nl *> symbol "}")
  where
    nl = lexeme $ optional newline

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","
