module Expression
  ( Expr (..),
    eval,
    parser,
    parseVar,
    parseRef,
  )
where

import Control.Monad.Combinators.Expr
import qualified Data.Map as Map
import Error
import Generator
import Literal (Literal)
import qualified Literal as Lit
import Parser
import Type (Type)
import qualified Type

data Expr
  = Lit Literal
  | Var Name
  | Ref Name
  | Fun Name [Expr]
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Int Expr
  deriving (Eq, Show)

parser :: Parser Expr
parser =
  makeExprParser term table <?> "expression"

term :: Parser Expr
term =
  choice
    [ parens parser,
      function,
      parseRef,
      parseVar,
      Lit <$> Lit.parser
    ]
  where
    function = try $ Fun <$> identifier <*> parens (sepBy parser comma)

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseRef :: Parser Expr
parseRef = char '&' >> Ref <$> identifier

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

eval :: Expr -> Gen (Type, Text)
eval expr = case expr of
  Lit x -> pure (Lit.type' x, showText x)
  Var name -> do
    var <- gets variables >>= maybe (throwError $ NotInScope name) pure . Map.lookup name
    pure (type' var, "H" <> (showText $ address var))
  Ref name -> do
    var <- gets variables >>= maybe (throwError $ NotInScope name) pure . Map.lookup name
    pure (type' var, showText $ address var)
  Fun name args ->
    maybe
      (throwError $ NotInScope name)
      ($ args)
      $ lookup name functions
  Neg x -> do
    (t, e) <- eval x
    case t of
      Type.Int -> pure (Type.Int, "-" <> e)
      Type.Real -> pure (Type.Real, "-" <> e)
      _ -> throwError $ Error
  Add x y -> additive x y "+"
  Sub x y -> additive x y "-"
  Mul x y -> do
    (tx, ex) <- eval x
    (ty, ey) <- eval y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, formatInfix ex ey "*")
      (Type.Int, Type.Real) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Int) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Real) -> pure (Type.Real, squareBrackets $ ex <> "*" <> ey <> "/1.")
      _ -> throwError Error
  Div x y -> do
    (tx, ex) <- eval x
    (ty, ey) <- eval y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, formatInfix ex ey "/")
      (Type.Real, Type.Int) -> pure (Type.Real, formatInfix ex ey "/")
      (Type.Real, Type.Real) -> pure (Type.Real, squareBrackets $ ex <> "/" <> ey <> "*1.")
      _ -> throwError Error
  Pow n e ->
    eval $
      ( if n < 0
          then Div (Lit $ Lit.Real 1.0)
          else id
      )
        $ case replicate (abs n) e of
          [] -> Lit $ Lit.Int 1
          x : xs -> foldr Mul x xs
  where
    additive x y s = do
      (tx, ex) <- eval x
      (ty, ey) <- eval y
      t <- case (tx, ty) of
        (Type.Int, Type.Int) -> pure Type.Int
        (Type.Real, Type.Real) -> pure Type.Real
        _ -> throwError $ Error
      pure (t, formatInfix ex ey s)
    formatInfix x y s = squareBrackets $ x <> s <> y

squareBrackets :: Text -> Text
squareBrackets x = "[" <> x <> "]"

functions :: [(Name, [Expr] -> Gen (Type, Text))]
functions =
  [ ("sin", included "SIN"),
    ("cos", included "COS"),
    ("tan", included "TAN"),
    ("asin", included "ASIN"),
    ("acos", included "ACOS"),
    ("atan", included "ATAN"),
    ("sqrt", included "SQRT"),
    ("round", included "ROUND"),
    ( "norm",
      \xs -> case map (Pow 2) xs of
        [] -> throwError $ Error
        x : xs' -> eval $ Fun "sqrt" $ [foldl Add x xs']
    )
  ]
  where
    included f = \case
      [x] -> do
        (t, e) <- eval x
        case t of
          Type.Real -> pure $ (Type.Real, f <> squareBrackets e)
          _ -> throwError $ Error
      _ -> throwError $ Error
