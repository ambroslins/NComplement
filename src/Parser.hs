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
import Data.Char (isSpace, isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Lexer
import Literal (Literal)
import qualified Literal as Lit
import Syntax
import Text.Megaparsec
  ( eof,
    lookAhead,
    match,
    notFollowedBy,
    oneOf,
    sepBy,
    takeWhile1P,
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
      function,
      reference,
      variable,
      Lit <$> literal
    ]
  where
    function = try $ Fun <$> name <*> parens (sepBy expr comma)

literal :: Parser Literal
literal =
  choice
    [ Lit.Bool True <$ symbol "True",
      Lit.Bool False <$ symbol "False",
      Lit.Real <$> try real,
      Lit.Int <$> natural
    ]

name :: Parser Name
name = Name <$> identifier

variable :: Parser Expr
variable = Var <$> name

reference :: Parser Expr
reference = char '&' >> Ref <$> name

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

value :: Parser (Value Expr)
value =
  match expr
    >>= pure . \case
      (v, Lit (Lit.Int _)) -> Val $ Text.takeWhile (not . isSpace) v
      (_, x) -> Expr x

-- Arguments

arg :: Parser (Name, Argument)
arg = do
  n <- name
  mtype <- optional $ symbol ":" >> parseType
  def <- optional $ symbol "=" >> literal
  desc <- optional (Text.pack <$> (char '(' >> manyTill charLiteral (char ')')))
  let t = fromMaybe Type.Real $ mtype <|> Lit.type' <$> def
  pure $ (n, Argument {argType = t, defaultLit = def, description = desc})

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
        get,
        set,
        assignment,
        scope,
        unsafe,
        jump,
        Codes <$> some code
      ]
  where
    ifStatement = do
      reserved "If"
      lhs <- expr
      ord <-
        choice
          [ EQ <$ symbol "=",
            LT <$ symbol "<",
            GT <$ symbol ">"
          ]
      rhs <- expr
      sThen <- statement
      sElse <- optional $ reserved "Else" *> statement
      pure $ If (lhs, ord, rhs) sThen sElse
    label = Label <$> name <* symbol ":"
    get = Get <$> try (sepBy1 name comma <* symbol "<-") <*> sepBy1 address comma
    set = Set <$> try (sepBy1 address comma <* symbol "<-") <*> sepBy1 expr comma
    assignment = do
      var <- name
      _ <- symbol "="
      e <- expr
      pure $ Assign var e
    scope = Scope <$> braces statements
    unsafe =
      Unsafe . Text.pack
        <$> (symbol "!" *> manyTill charLiteral (lookAhead (semicolon <|> eol)))
    jump = symbol "JUMP" >> Jump <$> name

code :: Parser (Code Expr)
code = Code <$> address <*> value

address :: Parser Address
address = lexeme $ takeWhile1P (Just "address") isUpper
