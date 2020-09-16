module Result
  ( Result (..),
    fromEither,
    toEither,
  )
where

import Data.Bifunctor

data Result a b = Err a | Ok b
  deriving (Eq, Ord, Show)

instance Functor (Result a) where
  fmap _ (Err x) = Err x
  fmap f (Ok x) = Ok (f x)

instance Bifunctor Result where
  bimap f _ (Err x) = Err (f x)
  bimap _ f (Ok x) = Ok (f x)

instance Semigroup b => Semigroup (Result a b) where
  Ok x <> Ok y = Ok (x <> y)
  Err x <> _ = Err x
  _ <> Err x = Err x

instance Monoid b => Monoid (Result a b) where
  mempty = Ok mempty
  mappend = (<>)

instance Applicative (Result a) where
  pure = Ok
  Err x <*> _ = Err x
  Ok f <*> x = fmap f x

instance Monad (Result a) where
  Err x >>= _ = Err x
  Ok x >>= k = k x

fromEither :: Either a b -> Result a b
fromEither (Left x) = Err x
fromEither (Right x) = Ok x

toEither :: Result a b -> Either a b
toEither (Err x) = Left x
toEither (Ok x) = Right x
