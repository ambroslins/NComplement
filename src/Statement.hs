module Statement
  ( Statement (..),
  )
where

import Expression (Expression, Name)
import Code (Code)

data Statement
  = Assign Name Expression
  | If Expression Statement Statement
  | Scope [Statement]
  | Code Code
  deriving (Eq, Show)
