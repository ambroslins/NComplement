module Parser where

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
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Lexer
import Literal (Literal)
import qualified Literal as Lit
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
      application,
      reference,
      symbol,
      Lit <$> literal
    ]
  where
    application = try $ App <$> name <*> parens (sepBy expr comma)

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
  def <- optional $ equal >> literal
  desc <- optional (Text.pack <$> (char '(' >> manyTill charLiteral (char ')')))
  let t = fromMaybe Type.Real $ mtype <|> Lit.type' <$> def
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
    choice
      [ ifStatement,
        try label,
        get,
        set,
        assignment,
        scope,
        unsafe,
        Codes <$> some code
      ]
  where
    ifStatement = do
      reserved "If"
      lhs <- expr
      ord <-
        choice
          [ EQ <$ equal,
            LT <$ lessThan,
            GT <$ greaterThan
          ]
      rhs <- expr
      sThen <- statement
      sElse <- optional $ reserved "Else" *> statement
      pure $ If (lhs, ord, rhs) sThen sElse
    label = Label <$> name <* colon
    get = Get <$> try (sepBy1 name comma <* leftArrow) <*> sepBy1 address comma
    set = Set <$> try (sepBy1 address comma <* leftArrow) <*> sepBy1 expr comma
    assignment = do
      var <- name
      equal
      e <- expr
      pure $ Assign var e
    scope = Scope <$> braces statements
    unsafe =
      Unsafe . splitCap (reference <|> symbol)
        <$> (exclamation *> takeWhileP Nothing (not . (`elem` [';', '\n'])))
    code = Code <$> address <*> expr

address :: Parser Address
address = lexeme $ Address <$> takeWhile1P (Just "address") isUpper
