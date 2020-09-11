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
  | If (Expr, Ordering, Expr) Statement (Maybe Statement)
  | Scope [Statement]
  | Unsafe Text
  | Label Name
  | Jump Name
  | Codes (NonEmpty Code)
  deriving (Eq, Show)

data Code
  = G Int
  | X Expr
  | Y Expr
  | Z Expr
  | U Expr
  | T Int
  | V Expr
  | M Int
  | F Expr
  | B Expr
  | E Expr
  | I Expr
  deriving (Eq, Show)
