module Error
  ( Error (..),
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Type (Type)

data Error
  = Error
  | UndefinedVar Text
  | UndefinedLabel Text
  | UndefinedFun Text
  | TypeMismatch Type Type
  | ParseError (ParseErrorBundle Text Void)
  deriving (Eq)

instance Show Error where
  show = \case
    Error -> "Error"
    UndefinedVar n -> "variable: " ++ show n ++ " is not defined"
    UndefinedLabel n -> "label: " ++ show n ++ " is not defined"
    UndefinedFun n -> "there is no function named " ++ show n
    TypeMismatch t1 t2 -> "can not match type " ++ show t1 ++ " with " ++ show t2
    ParseError p -> errorBundlePretty p
