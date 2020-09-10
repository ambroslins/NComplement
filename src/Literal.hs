module Literal
  ( Literal (..),
    type',
    toNC,
  )
where

import qualified NC
import Type (Type)
import qualified Type

data Literal
  = Real Double
  | Int Int
  | Bool Bool
  deriving (Eq, Show)

type' :: Literal -> Type
type' = \case
  Real _ -> Type.Real
  Int _ -> Type.Int
  Bool _ -> Type.Bool

toNC :: Literal -> NC.Expr
toNC = \case
  Real x -> NC.Real x
  Int x -> NC.Int x
  Bool x -> NC.Int $ if x then 1 else 0
