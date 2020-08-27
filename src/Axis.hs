module Axis where

import Parser

data Axis = X | Y | Z | U | V
  deriving (Eq, Show)

parser :: Parser Axis
parser =
  choice
    [ X <$ symbol "X",
      Y <$ symbol "Y",
      Z <$ symbol "Z",
      U <$ symbol "U",
      V <$ symbol "V"
    ]
