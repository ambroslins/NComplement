module Lexer
  ( Parser,
    getSourceLine,
    sc,
    scn,
    lexeme,
    lexeme',
    identifier,
    reserved,
    natural,
    integer,
    real,
    stringLiteral,
    parens,
    braces,
    semicolon,
    colon,
    comma,
    exclamation,
    plus,
    minus,
    asterisk,
    slash,
    circumflex,
    lessThan,
    equal,
    greaterThan,
    leftArrow,
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

getSourceLine :: Parser Pos
getSourceLine = sourceLine <$> getSourcePos

sc :: Parser ()
sc =
  Lex.space
    hspace1
    (Lex.skipLineComment "#")
    empty

scn :: Parser ()
scn =
  Lex.space
    (optional (char ';') >> space1)
    (Lex.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

lexeme' :: Parser a -> Parser a
lexeme' = Lex.lexeme scn

symbol :: Text -> Parser ()
symbol = void . Lex.symbol sc

identEnd :: Parser Char
identEnd = alphaNumChar <|> oneOf ['_', '\'']

identifier :: Parser Text
identifier = lexeme $ do
  x <- lowerChar <|> char '_'
  xs <- many $ identEnd
  pure $ Text.pack (x : xs)

reserved :: Text -> Parser ()
reserved x = lexeme $ string x >> notFollowedBy identEnd

natural :: (Integral a, Num a) => Parser a
natural = lexeme Lex.decimal

integer :: (Integral a, Num a) => Parser a
integer = lexeme $ Lex.signed sc natural

stringLiteral :: Parser Text
stringLiteral = between (char '"') (char '"') $ takeWhileP Nothing (/= '"')

real :: Parser Double
real = lexeme $ Lex.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (Lex.symbol scn "{") (Lex.symbol scn "}")

semicolon :: Parser ()
semicolon = symbol ";"

colon :: Parser ()
colon = symbol ":"

comma :: Parser ()
comma = symbol ","

exclamation :: Parser ()
exclamation = symbol "!"

plus :: Parser ()
plus = symbol "+"

minus :: Parser ()
minus = symbol "-"

asterisk :: Parser ()
asterisk = symbol "*"

slash :: Parser ()
slash = symbol "/"

circumflex :: Parser ()
circumflex = symbol "^"

lessThan :: Parser ()
lessThan = symbol "<"

equal :: Parser ()
equal = symbol "="

greaterThan :: Parser ()
greaterThan = symbol ">"

leftArrow :: Parser ()
leftArrow = symbol "<-"
