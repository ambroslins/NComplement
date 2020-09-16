module Literal
  ( Literal (..),
    type',
  )
where

import Data.Text (Text)
import Prettyprinter
import Type (Type)
import qualified Type

data Literal
  = Real Double
  | Int Int
  | Bool Bool
  | String Text
  deriving (Eq, Show)

type' :: Literal -> Type
type' = \case
  Real _ -> Type.Real
  Int _ -> Type.Int
  Bool _ -> Type.Bool
  String _ -> Type.String

instance Pretty Literal where
  pretty = \case
    Real x -> pretty x
    Int x -> pretty x
    Bool x -> pretty $ if x then 1 else (0 :: Int)
    String x -> pretty x
