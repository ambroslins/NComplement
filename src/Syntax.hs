module Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import Data.Text (Text)
import Literal (Literal)
import Type (Type)

data Argument = Argument
  { argDefault :: Maybe (Sign, Literal),
    argType :: Type,
    description :: Maybe Text
  }

data Program = Program
  { arguments :: [(Name, Argument)],
    body :: [Statement]
  }

data Expr
  = Lit Literal
  | Sym Name
  | Ref Name
  | App Name [Expr]
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Int Expr
  deriving (Eq, Show)

data Sign
  = Plus
  | Minus
  deriving (Eq, Show)

data Statement
  = Assign Name Expr
  | Get (NonEmpty Name) (NonEmpty Address)
  | Set (NonEmpty Address) (NonEmpty Expr)
  | Scope [Statement]
  | Unsafe [Either Text Expr]
  | Codes (NonEmpty Code)
  | If (Expr, Ordering, Expr) Statement (Maybe Statement)
  | Label Name
  deriving (Eq, Show)

data Code = Code Address Expr
  deriving (Eq, Show)

newtype Name = Name {unName :: Text}
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString s = Name $ fromString s

newtype Address = Address Text
  deriving (Eq, Ord, Show)

instance IsString Address where
  fromString s = Address $ fromString s

newtype Index = Index Int
  deriving (Eq, Ord, Show)

newtype Location = Location Int
  deriving (Eq, Ord, Show)
