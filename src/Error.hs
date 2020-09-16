module Error
  ( Error (..),
    CompileError (..),
  )
where

import Control.Exception (Exception (..))
import Data.Text (Text, unpack)
import Data.Void (Void)
import Syntax (Address (..), Name (..))
import Text.Megaparsec (Pos, unPos)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Type (Type)

data Error
  = ParseError (ParseErrorBundle Text Void)
  | InvalidExtension String
  | InvalidArguments
  | CompileError Pos Text CompileError

instance Exception Error

instance Show Error where
  show = \case
    ParseError e -> errorBundlePretty e
    InvalidExtension ext -> "Invalid extension: " ++ show ext ++ " should be \".nco\"."
    InvalidArguments -> "Invalid arguments."
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

data CompileError
  = OutOfIndices
  | OutOfLocations
  | UndefinedSymbol Name
  | NotAVariable Name
  | NotAFunction Name
  | AlreadyDefined Name
  | TypeMismatchNeg Type
  | TypeMismatchAdd Type Type
  | TypeMismatchSub Type Type
  | TypeMismatchMul Type Type
  | TypeMismatchDiv Type Type
  | TypeMismatchApp Name [Type] [Type]
  | TypeMismatchIf Type Type
  | TypeMismatchSet Address Type Type
  | TypeMismatchDef Name Type Type
  | NotAGetter Address
  | NotASetter Address
  | GetSetNotMatching
  deriving (Eq)

instance Show CompileError where
  show =
    (++ ".") . \case
      OutOfIndices -> "I am out of Indicies"
      OutOfLocations -> "I am out of Locations"
      UndefinedSymbol (Name n) -> "Symbol: " ++ show n ++ " is not defined"
      NotAVariable (Name n) -> "Symbol: " ++ show n ++ " is not a Variable"
      NotAFunction (Name n) -> "Symbol: " ++ show n ++ " is not a Function"
      AlreadyDefined (Name n) -> "Symbol: " ++ show n ++ " is already defined"
      TypeMismatchNeg t -> "I can not negate an expression of type: " ++ show t
      TypeMismatchAdd t1 t2 -> "I can not add " ++ show t1 ++ " with " ++ show t2
      TypeMismatchSub t1 t2 -> "I can not subtract " ++ show t2 ++ " from " ++ show t1
      TypeMismatchMul t1 t2 -> "I can not multiply " ++ show t1 ++ " with " ++ show t2
      TypeMismatchDiv t1 t2 -> "I can not divide " ++ show t1 ++ " by " ++ show t2
      TypeMismatchApp f expect got -> "Function " ++ show f ++ " is excpecting " ++ show expect ++ " but got " ++ show got
      TypeMismatchIf t1 t2 -> "I can not compare " ++ show t1 ++ " with " ++ show t2
      TypeMismatchSet (Address a) t1 t2 -> "Setting " ++ show a ++ " is expecting type " ++ show t1 ++ " but got " ++ show t2
      TypeMismatchDef (Name n) t1 t2 -> "Trying to define " ++ show n ++ " with type " ++ show t2 ++ " but it is already defined with type " ++ show t1
      NotAGetter (Address a) -> show a ++ " is not a valid Getter"
      NotASetter (Address a) -> show a ++ " is not a valid Setter"
      GetSetNotMatching -> "Targets and Sources do not match"
