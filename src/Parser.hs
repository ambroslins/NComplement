module Parser where

import Expression
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

expr =
  buildExpressionParser table term
    <?> "expression"

term :: Parsec String u Expression
term = parens expr <|> Lit <$> literal
  where
    literal =
      (symbol "True" *> pure (LitBool True))
        <|> (symbol "False" *> pure (LitBool False))
        <|> LitReal <$> try float
        <|> LitInteger <$> integer

table =
  [ [prefix "-" Neg, prefix "+" id],
    [binary "*" Mul AssocLeft, binary "/" Div AssocLeft],
    [binary "+" Add AssocLeft, binary "-" Sub AssocLeft],
    [binary "=" Eq AssocNone, binary "<" Lt AssocNone, binary ">" Gt AssocNone]
  ]
  where
    binary name fun assoc = Infix (do reservedOp name; return fun) assoc
    prefix name fun = Prefix (do reservedOp name; return fun)
    postfix name fun = Postfix (do reservedOp name; return fun)

lexer =
  Token.makeTokenParser
    emptyDef
      { Token.commentLine = "--",
        Token.opStart = oneOf "+-*/=<>"
      }

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

natural = Token.natural lexer

integer = Token.integer lexer

float = Token.float lexer

symbol = Token.symbol lexer

lexeme = Token.lexeme lexer

whiteSpace = Token.whiteSpace lexer

parens = Token.parens lexer

braces = Token.braces lexer

angles = Token.angles lexer

brackets = Token.brackets lexer

semi = Token.semi lexer

comma = Token.comma lexer

colon = Token.colon lexer

semiSep = Token.semiSep lexer
