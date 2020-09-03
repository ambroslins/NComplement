module Parser where

import Control.Monad.Combinators
  ( choice,
    many,
    manyTill,
    optional,
    (<|>),
  )
import Control.Monad.Combinators.Expr
import Control.Monad.Combinators.NonEmpty (some)
import qualified Data.Text as Text
import Lexer
import qualified Literal as Lit
import Syntax
import Text.Megaparsec
  ( lookAhead,
    sepBy,
    sepEndBy,
    try,
    (<?>),
  )
import Text.Megaparsec.Char
  ( char,
    eol,
  )

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
      literal
    ]
  where
    function = try $ Fun <$> identifier <*> parens (sepBy expr comma)
    literal =
      Lit
        <$> choice
          [ Lit.Bool True <$ symbol "True",
            Lit.Bool False <$ symbol "False",
            Lit.Real <$> try real,
            Lit.Int <$> integer
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
    prefix name f = Prefix (f <$ symbol name)
    infixLeft name f = InfixL (f <$ symbol name)

-- Arguments

argument :: Parser Argument
argument = do
  name <- identifier
  desc <- optional (Text.pack <$> (char '(' >> manyTill charLiteral (char ')')))
  pure (name, desc)

arguments :: Parser [Argument]
arguments = symbol "Args" >> parens (sepBy argument sep)
  where
    sep = comma >> many (lexeme eol)

-- Statement

statements :: Parser [Statement]
statements = sepEndBy statement sep
  where
    sep = some $ lexeme (semicolon <|> eol)

statement :: Parser Statement
statement =
  choice
    [ ifStatement,
      try label,
      assignment,
      scope,
      unsafe,
      jump
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
      reserved "="
      e <- expr
      pure $ Assign var e
    scope = Scope <$> braces statements
    unsafe = Unsafe . Text.pack <$> (symbol "!" *> manyTill charLiteral (lookAhead (semicolon <|> eol)))
    jump = symbol "JUMP" >> Jump <$> identifier
