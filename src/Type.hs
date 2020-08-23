module Type (Type (..), Error (..)) where

data Type
  = Bool
  | Integer
  | Real
  deriving (Eq, Show)

data Error
  = Error
  | NotInScope String
