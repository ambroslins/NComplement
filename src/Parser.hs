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
import Control.Monad.Combinators.NonEmpty (some)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Lexer
import Literal (Literal)
import qualified Literal as Lit
import Syntax
import Text.Megaparsec
  ( eof,
    lookAhead,
    notFollowedBy,
    oneOf,
    sepBy,
    sepEndBy,
    try,
    (<?>),
  )
import Text.Megaparsec.Char
  ( char,
    eol,
  )
import Text.Megaparsec.Char.Lexer
  ( indentGuard,
    indentLevel,
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
      function,
      reference,
      variable,
      Lit <$> literal
    ]
  where
    function = try $ Fun <$> identifier <*> parens (sepBy expr comma)

literal :: Parser Literal
literal =
  choice
    [ Lit.Bool True <$ symbol "True",
      Lit.Bool False <$ symbol "False",
      Lit.Real <$> try real,
      Lit.Int <$> natural
    ]

variable :: Parser Expr
variable = Var <$> identifier

reference :: Parser Expr
reference = char '&' >> Ref <$> identifier

table :: [[Operator Parser Expr]]
table =
  [ [ prefix "-" Neg,
      prefix "+" id
    ],
    [ Postfix (Pow <$> (symbol "^" *> integer))
    ],
    [ infixLeft "*" (Mul),
      infixLeft "/" (Div)
    ],
    [ infixLeft "+" (Add),
      infixLeft "-" (Sub)
    ]
  ]
  where
    prefix sym f = Prefix (f <$ symbol sym <* notFollowedBy (oneOf ['-', '+']))
    infixLeft sym f = InfixL (f <$ symbol sym)

-- Arguments

arg :: Parser (Name, Argument)
arg = do
  name <- identifier
  mtype <- optional $ symbol ":" >> parseType
  val <- optional $ symbol "=" >> literal
  desc <- optional (Text.pack <$> (char '(' >> manyTill charLiteral (char ')')))
  let t = fromMaybe Type.Real $ mtype <|> Lit.type' <$> val
  pure $ (name, Argument {argType = t, value = val, description = desc})

parseType :: Parser Type
parseType =
  choice
    [ Type.Real <$ symbol "Real",
      Type.Int <$ symbol "Int",
      Type.Bool <$ symbol "Bool"
    ]

args :: Parser [(Name, Argument)]
args =
  symbol "Args"
    >> parens (sepBy arg sep)
    <* some (lexeme (semicolon <|> eol))
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
        assignment,
        scope,
        unsafe,
        jump,
        Code <$> code
      ]
  where
    ifStatement = do
      reserved "If"
      lhs <- expr
      comp <- choice [Eq <$ symbol "=", Lt <$ symbol "<", Gt <$ symbol ">"]
      rhs <- expr
      sThen <- statement
      sElse <- optional $ reserved "Else" *> statement
      pure $ If (lhs, comp, rhs) sThen sElse
    label = Label <$> identifier <* symbol ":"
    assignment = do
      var <- identifier
      _ <- symbol "="
      e <- expr
      pure $ Assign var e
    scope = Scope <$> braces statements
    unsafe =
      Unsafe . Text.pack
        <$> (symbol "!" *> manyTill charLiteral (lookAhead (semicolon <|> eol)))
    jump = symbol "JUMP" >> Jump <$> identifier

code :: Parser Code
code = do
  _ <- symbol "G00"
  i <- indentLevel
  x <- p
  xs <- many $ try $ indentGuard scn EQ i >> p
  pure $ G00 (x :| xs)
  where
    p :: Parser (NonEmpty (Axis, Expr))
    p = some $ (,) <$> axis <*> expr

axis :: Parser Axis
axis =
  choice $
    map
      (\a -> a <$ symbol (Text.pack $ show a))
      [minBound .. maxBound]
