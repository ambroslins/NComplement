module AST where

data Statement
  = Assignment Name Expression
  | If Expression [Statement] [Statement]
  deriving (Show)

type Name = String

data Expression
  = Lit Literal
  | Var Name
  | Ref Name
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
    Ref n -> '&' : show n
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
