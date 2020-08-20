module Expression where

type Name = String

data Expression
  = Lit Literal
  | Var Name
  | Neg Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Eq Expression Expression
  | Lt Expression Expression
  | Gt Expression Expression

instance Show Expression where
  show = \case
    Lit l -> show l
    Var n -> n
    Neg x -> '-' : show x
    Add x y -> infixOp "+" x y
    Sub x y -> infixOp "-" x y
    Mul x y -> infixOp "*" x y
    Div x y -> infixOp "/" x y
    Eq x y -> infixOp "=" x y
    Lt x y -> infixOp "<" x y
    Gt x y -> infixOp ">" x y
    where
      infixOp s x y = "(" ++ show x ++ s ++ show y ++ ")"

data Literal
  = LitBool Bool
  | LitInteger Integer
  | LitReal Double

instance Show Literal where
  show = \case
    LitBool x -> show x
    LitInteger x -> show x
    LitReal x -> show x

data Type
  = Bool
  | Integer
  | Real
  deriving (Eq, Show)

data TypeError
  = TypeError
  | NotInScope Name
  deriving (Show)

type Env = [(Name, Type)]

typeof :: Env -> Expression -> Either TypeError Type
typeof env = \case
  Lit l -> pure $ case l of
    LitBool _ -> Bool
    LitInteger _ -> Integer
    LitReal _ -> Real
  Var v -> case lookup v env of
    Nothing -> Left $ NotInScope v
    Just t -> pure t
  Neg x -> do
    t <- typeof env x
    case t of
      Integer -> pure Integer
      Real -> pure Real
      _ -> Left TypeError
  Add x y -> additive x y
  Sub x y -> additive x y
  Mul x y -> multiplicative x y
  Div x y -> multiplicative x y
  Eq x y -> do
    tx <- typeof env x
    ty <- typeof env y
    if tx == ty then pure Bool else Left TypeError
  Lt x y -> comparative x y
  Gt x y -> comparative x y
  where
    additive x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (Integer, Integer) -> pure Integer
        (Real, Real) -> pure Real
        _ -> Left TypeError
    multiplicative x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (Integer, Integer) -> pure Integer
        (Integer, Real) -> pure Real
        (Real, Integer) -> pure Real
        (Real, Real) -> pure Real
        _ -> Left TypeError
    comparative x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (Integer, Integer) -> pure Bool
        (Real, Real) -> pure Bool
        _ -> Left TypeError
