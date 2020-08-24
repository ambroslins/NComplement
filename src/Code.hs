module Code where

import Data.List.NonEmpty (NonEmpty)
import Expression (Expression)

data Code
  = G00 (NonEmpty (Axis, Expression))
  deriving (Eq, Show)

data Axis = X | Y | Z | U | V
  deriving (Eq, Show)
