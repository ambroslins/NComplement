module Statement
  ( Statement (..),
  )
where

import Expression (Expression, Name)

data Statement
  = Assign Name Expression
  | If Expression [Statement] [Statement]
  deriving (Eq, Show)
