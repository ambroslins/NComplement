module Type
  ( Type (..),
    parser,
  )
where

import Parser

data Type
  = Bool
  | Int
  | Real
  deriving (Eq, Show)

parser :: Parser Type
parser =
  choice
    [ Bool <$ symbol "Bool",
      Int <$ symbol "Integer",
      Real <$ symbol "Real"
    ]
