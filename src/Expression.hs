module Expression where

data Expression
  = Lit Literal
  | Neg Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Eq Expression Expression
  | Lt Expression Expression
  | Gt Expression Expression

instance Show Expression where
  show e = case e of
        Lit l -> case l of
          LitBool x -> show x
          LitInteger x -> show x
          LitReal x -> show x
        Neg x -> '-':show x
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
  deriving (Show)

data Type
  = Bool
  | Integer
  | Real
  deriving (Eq, Show)

data TypeError = TypeError deriving (Show)

typeof :: Expression -> Either TypeError Type
typeof e = case e of
  Lit l -> pure $ case l of
    LitBool _ -> Bool
    LitInteger _ -> Integer
    LitReal _ -> Real
  Neg x -> do
    t <- typeof x
    case t of
      Integer -> pure Integer
      Real -> pure Real
      _ -> Left TypeError
  Add x y -> additive x y
  Sub x y -> additive x y
  Mul x y -> multiplicative x y
  Div x y -> multiplicative x y
  Eq x y -> do
    tx <- typeof x
    ty <- typeof y
    if tx == ty then pure Bool else Left TypeError
  Lt x y -> comparative x y
  Gt x y -> comparative x y
  where
    additive x y = do
      tx <- typeof x
      ty <- typeof y
      case (tx, ty) of
        (Integer, Integer) -> pure Integer
        (Real, Real) -> pure Real
        _ -> Left TypeError
    multiplicative x y = do
      tx <- typeof x
      ty <- typeof y
      case (tx, ty) of
        (Integer, Integer) -> pure Integer
        (Integer, Real) -> pure Real
        (Real, Integer) -> pure Real
        (Real, Real) -> pure Real
        _ -> Left TypeError
    comparative x y = do
      tx <- typeof x
      ty <- typeof y
      case (tx, ty) of
        (Integer, Integer) -> pure Bool
        (Real, Real) -> pure Bool
        _ -> Left TypeError
