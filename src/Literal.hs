module Literal
  ( Literal (..),
    type',
    parser,
  )
where

import Parser
import Type (Type)
import qualified Type

data Literal
  = Real Double
  | Int Int
  | Bool Bool
  deriving (Eq)

instance Show Literal where
  show = \case
    Real x -> show x
    Int x -> show x
    Bool x -> if x then "1" else "0"

type' :: Literal -> Type
type' = \case
  Real _ -> Type.Real
  Int _ -> Type.Int
  Bool _ -> Type.Bool

parser :: Parser Literal
parser =
  choice
    [ Bool True <$ symbol "True",
      Bool False <$ symbol "False",
      Real <$> try real,
      Int <$> integer
    ]
