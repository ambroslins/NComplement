module Statement where

import Expression

data Statement
  = Assignment Name Expression
  | If Expression [Statement] [Statement]
  deriving (Show)
