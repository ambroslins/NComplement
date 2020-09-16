module Result
  ( Result (..),
    fromEither,
    toEither,
  )
where

data Result a b = Err a | Ok b
  deriving (Eq, Ord, Show)

instance Functor (Result a) where
  fmap _ (Err x) = Err x
  fmap f (Ok x) = Ok (f x)

instance Semigroup b => Semigroup (Result a b) where
  Ok x <> Ok y = Ok (x <> y)
  Err x <> _ = Err x
  _ <> Err x = Err x

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
