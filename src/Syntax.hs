module Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Literal (Literal)
import Type (Type)

type Name = Text

data Argument = Argument
  { value :: Maybe Literal,
    argType :: Type,
    description :: Maybe Text
  }

data Program = Program
  { arguments :: [(Name, Argument)],
    body :: [Statement]
  }

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

data Statement
  = Assign Name Expr
  | If (Expr, Comparison, Expr) Statement (Maybe Statement)
  | Scope [Statement]
  | Unsafe Text
  | Label Name
  | Jump Name
  | Code Code
  deriving (Eq, Show)

data Comparison
  = Eq
  | Lt
  | Gt
  deriving (Eq)

instance Show Comparison where
  show = \case
    Eq -> "="
    Lt -> "<"
    Gt -> ">"

data Code
  = G00 (NonEmpty (NonEmpty (Axis, Expr)))
  deriving (Eq, Show)

data Axis = X | Y | Z | U | V
  deriving (Eq, Ord, Show, Enum, Bounded)
