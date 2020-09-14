module Error
  ( Error (..),
    Error' (..),
  )
where

import Control.Exception (Exception (..))
import Data.Text (Text, unpack)
import Data.Void (Void)
import Syntax (Name)
import Text.Megaparsec (Pos, unPos)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Type (Type)

data Error
  = ParseError (ParseErrorBundle Text Void)
  | CompileError Pos Text Error'

data Error'
  = Error
  | UndefinedVar Name
  | UndefinedLabel Name
  | UndefinedFun Name
  | TypeMismatch Type Type
  deriving (Eq)

instance Exception Error

instance Show Error where
  show = \case
    ParseError e -> errorBundlePretty e
    CompileError pos line e ->
      unlines
        [ "Compile Error at Line " ++ show (unPos pos),
          blank,
          p ++ " | " ++ unpack line,
          blank,
          show e
        ]
      where
        p = show (unPos pos)
        blank = (' ' <$ p) ++ " |"

instance Exception Error'

instance Show Error' where
  show = \case
    Error -> "Error"
    UndefinedVar n -> "variable: " ++ show n ++ " is not defined"
    UndefinedLabel n -> "label: " ++ show n ++ " is not defined"
    UndefinedFun n -> "there is no function named " ++ show n
    TypeMismatch t1 t2 -> "can not match type " ++ show t1 ++ " with " ++ show t2
