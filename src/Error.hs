module Error
  ( Error (..),
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

data Error
  = Error
  | NotInScope Text
  | ParseError (ParseErrorBundle Text Void)
  deriving (Eq)

instance Show Error where
  show = \case
    Error -> "Error"
    NotInScope n -> "Identifier " ++ show n ++ " not in Scope"
    ParseError p -> errorBundlePretty p
