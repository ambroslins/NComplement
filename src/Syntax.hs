module Syntax where

import Data.Text (Text)
import Literal (Literal)

type Name = Text

type Argument = (Name, Maybe Text)

data Program = Program
  { arguments :: [Argument],
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
