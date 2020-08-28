module Literal
  ( Literal (..),
    parser,
    typeOf,
  )
where

import Parser
import Type (Type)
import qualified Type

data Literal
  = Bool Bool
  | Integer Integer
  | Real Double
  deriving (Eq)

instance Show Literal where
  show = \case
    Bool x -> if x then "1" else "0"
    Integer x -> show x
    Real x -> show x

typeOf :: Literal -> Type
typeOf = \case
  Bool _ -> Type.Bool
  Integer _ -> Type.Integer
  Real _ -> Type.Real

parser :: Parser Literal
parser =
  choice
    [ Bool True <$ symbol "True",
      Bool False <$ symbol "False",
      Real <$> try real,
      Integer <$> integer
    ]
