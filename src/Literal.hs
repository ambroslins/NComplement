module Literal
  ( Literal (..),
    type',
  )
where

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

