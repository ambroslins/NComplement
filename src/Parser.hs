module Parser where

import Control.Monad.Identity (Identity)
import Expression
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

expr :: Parsec String st Expression
expr =
  buildExpressionParser table term
    <?> "expression"

term :: Parsec String st Expression
term = parens expr <|> Lit <$> literal <|> variabel
  where
    literal =
      (symbol "True" *> pure (LitBool True))
        <|> (symbol "False" *> pure (LitBool False))
        <|> LitReal <$> try float
        <|> LitInteger <$> integer
    variabel = Var <$> identifier

table :: OperatorTable String st Identity Expression
table =
  [ [prefix "-" Neg, prefix "+" id],
    [binary "*" Mul AssocLeft, binary "/" Div AssocLeft],
    [binary "+" Add AssocLeft, binary "-" Sub AssocLeft],
    [binary "=" Eq AssocNone, binary "<" Lt AssocNone, binary ">" Gt AssocNone]
  ]
  where
    binary name fun assoc = Infix (do reservedOp name; return fun) assoc
    prefix name fun = Prefix (do reservedOp name; return fun)

lexer :: Token.GenTokenParser String st Identity
lexer =
  Token.makeTokenParser
    emptyDef
      { Token.commentLine = "--",
        Token.identStart = lower <|> char '_',
        Token.opStart = oneOf "+-*/=<>"
      }

identifier :: Parsec String st String
identifier = Token.identifier lexer

reservedOp :: String -> Parsec String st ()
reservedOp = Token.reservedOp lexer

natural :: Parsec String u Integer
natural = Token.natural lexer

integer :: Parsec String u Integer
integer = Token.integer lexer

float :: Parsec String u Double
float = Token.float lexer

symbol :: String -> Parsec String u String
symbol = Token.symbol lexer

lexeme :: Parsec String u a -> Parsec String u a
lexeme = Token.lexeme lexer

whiteSpace :: Parsec String u ()
whiteSpace = Token.whiteSpace lexer

parens :: Parsec String u a -> Parsec String u a
parens = Token.parens lexer

braces :: Parsec String u a -> Parsec String u a
braces = Token.braces lexer

angles :: Parsec String u a -> Parsec String u a
angles = Token.angles lexer

brackets :: Parsec String u a -> Parsec String u a
brackets = Token.brackets lexer

semi :: Parsec String u String
semi = Token.semi lexer

comma :: Parsec String u String
comma = Token.comma lexer

colon :: Parsec String u String
colon = Token.colon lexer

semiSep :: Parsec String u a -> Parsec String u [a]
semiSep = Token.semiSep lexer
