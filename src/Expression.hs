module Expression
  ( Name,
    Expression (..),
    Operator (..),
    Literal (..),
  )
where

import Data.Text (Text)

type Name = Text

data Expression
  = Lit Literal
  | Var Name
  | Ref Name
  | Neg Expression
  | Op Operator Expression Expression
  deriving (Eq, Show)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Gt
  deriving (Eq, Show)

data Literal
  = LitBool Bool
  | LitInteger Integer
  | LitReal Double
  deriving (Eq)

instance Show Literal where
  show = \case
    LitBool x -> show x
    LitInteger x -> show x
    LitReal x -> show x
