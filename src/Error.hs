module Error
  ( Error (..),
  )
where

import Data.Text (Text)

data Error
  = Error
  | NotInScope Text
  deriving (Eq, Show)
