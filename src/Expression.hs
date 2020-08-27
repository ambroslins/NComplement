module Expression
  ( Name,
    Expr (..),
    Literal (..),
    compile,
    parser,
    Variable (..),
  )
where

import Compiler
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Literal (Literal (..))
import qualified Literal as Lit
import Parser
import Type (Type)
import qualified Type

type Name = Text

data Expr
  = Lit Literal
  | Var Name
  | Ref Name
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

parser :: Parser Expr
parser =
  makeExprParser term table <?> "expression"

term :: Parser Expr
term = parens parser <|> literal <|> reference <|> variabel
  where
    literal =
      Lit
        <$> ( Lit.Bool True <$ symbol "True"
                <|> Lit.Bool False <$ symbol "False"
                <|> Lit.Real <$> try real
                <|> Lit.Integer <$> integer
            )
    reference = char '&' >> Ref <$> identifier
    variabel = Var <$> identifier

table :: [[Operator Parser Expr]]
table =
  [ [ prefix "-" Neg,
      prefix "+" id
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

compile :: Expr -> Compiler (Type, Text)
compile expr = case expr of
  Lit lit -> pure (Lit.typeOf lit, showText lit)
  Var name -> do
    var <- lookupVar name
    pure (type' var, "H" <> (showText $ address var))
  Ref name -> do
    var <- lookupVar name
    pure (type' var, showText $ address var)
  Neg x -> do
    (t, e) <- compile x
    case t of
      Type.Integer -> pure (Type.Integer, "-" <> showText e)
      Type.Real -> pure (Type.Real, "-" <> e)
      _ -> throwError $ Error
  Add x y -> additive x y "+"
  Sub x y -> additive x y "-"
  Mul x y -> do
    (tx, ex) <- compile x
    (ty, ey) <- compile y
    case (tx, ty) of
      (Type.Integer, Type.Integer) -> pure (Type.Integer, formatInfix ex ey "*")
      (Type.Integer, Type.Real) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Integer) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Real) -> pure (Type.Real, inSquare $ ex <> "*" <> ey <> "/1.")
      _ -> throwError Error
  Div x y -> do
    (tx, ex) <- compile x
    (ty, ey) <- compile y
    case (tx, ty) of
      (Type.Integer, Type.Integer) -> pure (Type.Integer, formatInfix ex ey "/")
      (Type.Real, Type.Integer) -> pure (Type.Real, formatInfix ex ey "/")
      (Type.Real, Type.Real) -> pure (Type.Real, inSquare $ ex <> "/" <> ey <> "*1.")
      _ -> throwError Error
  where
    additive x y s = do
      (tx, ex) <- compile x
      (ty, ey) <- compile y
      t <- case (tx, ty) of
        (Type.Integer, Type.Integer) -> pure Type.Integer
        (Type.Real, Type.Real) -> pure Type.Real
        _ -> throwError $ Error
      pure (t, formatInfix ex ey s)
    formatInfix x y s = inSquare $ x <> s <> y
    inSquare s = "[" <> s <> "]"
