module Statement where

import Expression

data Statement
  = Code Code
  | Assignment Name Expression
  | If Expression [Statement] [Statement]

data Code
