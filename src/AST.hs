module AST where

data Statement
  = Assignment Name Expression
  | If Expression [Statement] [Statement]
  deriving (Show)

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
  = TypeBool
  | TypeInteger
  | TypeReal
  deriving (Eq, Show)

data TypeError
  = TypeMissmatch
  | NotInScope Name
  deriving (Show)

type Env = [(Name, Type)]

typeof :: Env -> Expression -> Either TypeError Type
typeof env = \case
  Lit l -> pure $ case l of
    LitBool _ -> TypeBool
    LitInteger _ -> TypeInteger
    LitReal _ -> TypeReal
  Var v -> case lookup v env of
    Nothing -> Left $ NotInScope v
    Just t -> pure t
  Neg x -> do
    t <- typeof env x
    case t of
      TypeInteger -> pure TypeInteger
      TypeReal -> pure TypeReal
      _ -> Left TypeMissmatch
  Add x y -> additive x y
  Sub x y -> additive x y
  Mul x y -> multiplicative x y
  Div x y -> multiplicative x y
  Eq x y -> do
    tx <- typeof env x
    ty <- typeof env y
    if tx == ty then pure TypeBool else Left TypeMissmatch
  Lt x y -> comparative x y
  Gt x y -> comparative x y
  where
    additive x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (TypeInteger, TypeInteger) -> pure TypeInteger
        (TypeReal, TypeReal) -> pure TypeReal
        _ -> Left TypeMissmatch
    multiplicative x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (TypeInteger, TypeInteger) -> pure TypeInteger
        (TypeInteger, TypeReal) -> pure TypeReal
        (TypeReal, TypeInteger) -> pure TypeReal
        (TypeReal, TypeReal) -> pure TypeReal
        _ -> Left TypeMissmatch
    comparative x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (TypeInteger, TypeInteger) -> pure TypeBool
        (TypeReal, TypeReal) -> pure TypeBool
        _ -> Left TypeMissmatch
