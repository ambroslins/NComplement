module Type
  ( Type (..),
    parser,
  )
where

import Parser

data Type
  = Bool
  | Integer
  | Real
  deriving (Eq, Show)

parser :: Parser Type
parser =
  choice
    [ Bool <$ symbol "Bool",
      Integer <$ symbol "Integer",
      Real <$ symbol "Real"
    ]
