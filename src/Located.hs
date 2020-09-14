module Located
  ( Located (..),
    Pos,
    unPos,
  )
where

import Text.Megaparsec (Pos, unPos)

data Located a = At Pos a
  deriving (Eq, Show)

instance Functor Located where
  fmap f (At p x) = At p (f x)
