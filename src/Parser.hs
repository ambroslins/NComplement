module Parser where

import Control.Monad (liftM2)
import Control.Monad.Combinators
  ( choice,
    many,
    manyTill,
    option,
    optional,
    (<|>),
  )
import Control.Monad.Combinators.Expr
import Control.Monad.Combinators.NonEmpty
  ( sepBy1,
    some,
  )
import Data.Char (isUpper)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Lexer
import Literal (Literal)
import qualified Literal as Lit
import Located
import Replace.Megaparsec (splitCap)
import Syntax
import Text.Megaparsec
  ( eof,
    sepBy,
    takeWhile1P,
    takeWhileP,
    try,
    (<?>),
  )
import Text.Megaparsec.Char
  ( char,
    eol,
  )
import Type (Type)
import qualified Type

-- Program

program :: Parser Program
program = Program <$> option [] args <*> statements <* (scn >> eof)

-- Expression

expr :: Parser Expr
expr =
  makeExprParser term table <?> "expression"

term :: Parser Expr
term =
  choice
    [ parens expr,
      apply,
      reference,
      symbol,
      Lit <$> literal
    ]
  where
    apply = try $ App <$> name <*> parens (sepBy expr comma)

literal :: Parser Literal
literal =
  choice
    [ Lit.Bool True <$ reserved "True",
      Lit.Bool False <$ reserved "False",
      Lit.Real <$> try real,
      Lit.Int <$> natural,
      Lit.String . Text.pack <$> (char '"' *> manyTill charLiteral (char '"'))
    ]

name :: Parser Name
name = Name <$> identifier

symbol :: Parser Expr
symbol = Sym <$> name

reference :: Parser Expr
reference = char '&' >> Ref <$> name

table :: [[Operator Parser Expr]]
table =
  [ [ prefix minus Neg,
      prefix plus id
    ],
    [ Postfix (Pow <$> (circumflex *> integer))
    ],
    [ infixLeft asterisk Mul,
      infixLeft slash Div
    ],
    [ infixLeft plus Add,
      infixLeft minus Sub
    ]
  ]
  where
    prefix p f = Prefix (f <$ p)
    infixLeft p f = InfixL (f <$ p)

-- Arguments

arg :: Parser (Name, Argument)
arg = do
  n <- name
  mtype <- optional $ colon >> parseType
  def <- optional $ do
    equal
    sign <- option Plus (Minus <$ minus <|> Plus <$ plus)
    lit <- literal
    pure (sign, lit)
  desc <- optional (Text.pack <$> (char '(' >> manyTill charLiteral (char ')')))
  let t = fromMaybe Type.Real $ mtype <|> Lit.type' . snd <$> def
  pure $ (n, Argument {argType = t, argDefault = def, description = desc})

parseType :: Parser Type
parseType =
  choice
    [ Type.Real <$ reserved "Real",
      Type.Int <$ reserved "Int",
      Type.Bool <$ reserved "Bool"
    ]

args :: Parser [(Name, Argument)]
args =
  lexeme' $
    reserved "Args"
      >> parens (sepBy arg sep)
  where
    sep = comma >> many (lexeme eol)

-- Statement

statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement =
  lexeme' $
    At <$> getSourceLine
      <*> choice
        [ parseIf,
          Get <$> try (name `sepBy1` comma <* leftArrow)
            <*> address `sepBy1` comma,
          Set <$> try (address `sepBy1` comma <* leftArrow)
            <*> expr `sepBy1` comma,
          Label <$> try (name <* colon),
          Assign <$> name <* equal <*> expr,
          Scope <$> braces statements,
          try $ Call <$> address <*> parens (expr `sepBy` comma),
          Codes <$> some (Code <$> address <*> expr),
          unsafe
        ]
  where
    parseIf = do
      reserved "If"
      lhs <- expr
      ord <- EQ <$ equal <|> LT <$ lessThan <|> GT <$ greaterThan
      rhs <- expr
      sThen <- statement
      sElse <- optional $ reserved "Else" *> statement
      pure $ If (lhs, ord, rhs) sThen sElse
    unsafe =
      Unsafe . splitCap (reference <|> symbol)
        <$> (exclamation *> takeWhileP Nothing (not . (`elem` [';', '\n'])))

address :: Parser Address
address = lexeme $ Address <$> takeWhile1P (Just "address") isUpper
