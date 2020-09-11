module Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import Data.Text (Text)
import Literal (Literal)
import Type (Type)

newtype Name = Name {unName :: Text}
  deriving (Eq, Ord)

instance Show Name where
  show (Name x) = show x

instance IsString Name where
  fromString s = Name $ fromString s

data Argument = Argument
  { defaultLit :: Maybe Literal,
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
  | Codes (NonEmpty (Code Expr))
  | Get Name Address
  | Set Address Expr
  deriving (Eq, Show)

data Code a = Code Address (Value a)
  deriving (Eq, Show)

type Address = Text

data Value a = Val Text | Expr a
  deriving (Eq, Show)
