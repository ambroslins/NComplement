module NC where

import Data.Text (Text)
import qualified Data.Text as Text

data Statement
  = Codes [Code]
  | N RecordNumber
  | Assign Index Expr
  | IF (Expr, Ordering, Expr) (Maybe RecordNumber, Maybe RecordNumber)
  | JUMP RecordNumber
  | CRT Text
  | Definiton Index (Either Int Double) Text
  | Escape Text
  deriving (Eq, Show)

data Code
  = G Int
  | X Expr
  | Y Expr
  | Z Expr
  | U Expr
  | V Expr
  | P RecordNumber
  | L Int
  | C Int
  | M Int
  | Q Text
  | F Expr
  deriving (Eq, Show)

data Expr
  = Int Int
  | Real Double
  | Var Index
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Fun Function Expr
  deriving (Eq, Show)

data Function
  = SIN
  | COS
  | TAN
  | ASIN
  | ACOS
  | ATAN
  | SQRT
  | ROUND
  deriving (Eq, Show)

showText :: Show a => a -> Text
showText = Text.pack . show

pad0 :: Show a => Int -> a -> Text
pad0 n = Text.justifyRight n '0' . showText

newtype Index = Index {unIndex :: Int}
  deriving (Eq, Ord)

instance Show Index where
  show (Index x) = Text.unpack $ pad0 3 x

newtype RecordNumber = RecordNumber Int
  deriving (Eq, Ord)

instance Show RecordNumber where
  show (RecordNumber x) = Text.unpack $ pad0 4 x
