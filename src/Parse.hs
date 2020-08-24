module Parse where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Expression (Expression)
import qualified Expression as Expr
import Statement (Statement)
import qualified Statement as Stmt
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

statements :: Parser [Statement]
statements = sepEndBy statement sep
  where
    sep = lexeme (void semicolon <|> void newline) >> many (lexeme newline)

statement :: Parser Statement
statement = ifStatement <|> assignment
  where
    ifStatement = do
      reserved "If"
      cond <- expression
      sThen <- braces statements
      sElse <- option [] $ reserved "Else" *> braces statements
      pure $ Stmt.If cond sThen sElse
    assignment = do
      var <- identifier
      reserved "="
      e <- expression
      pure $ Stmt.Assign var e

expression :: Parser Expression
expression =
  makeExprParser term table <?> "expression"

term :: Parser Expression
term = parens expression <|> literal <|> reference <|> variabel
  where
    literal =
      Expr.Lit
        <$> ( Expr.LitBool True <$ symbol "True"
                <|> Expr.LitBool False <$ symbol "False"
                <|> Expr.LitReal <$> try real
                <|> Expr.LitInteger <$> integer
            )
    reference = char '&' >> Expr.Ref <$> identifier
    variabel = Expr.Var <$> identifier

table :: [[Operator Parser Expression]]
table =
  [ [ prefix "-" Expr.Neg,
      prefix "+" id
    ],
    [ infixLeft "*" (Expr.Op Expr.Mul),
      infixLeft "/" (Expr.Op Expr.Div)
    ],
    [ infixLeft "+" (Expr.Op Expr.Add),
      infixLeft "-" (Expr.Op Expr.Sub)
    ],
    [ infixNone "=" (Expr.Op Expr.Eq),
      infixNone "<" (Expr.Op Expr.Lt),
      infixNone ">" (Expr.Op Expr.Gt)
    ]
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
braces = between (symbol "{" <* nl) (nl *> symbol "}")
  where
    nl = lexeme $ optional newline

semicolon :: Parser String
semicolon = symbol ";"

comma :: Parser String
comma = symbol ","
