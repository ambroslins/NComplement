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

printStmts :: [Statement] -> Text
printStmts = Text.unlines . map (<> ";") . map printStmt

printStmt :: Statement -> Text
printStmt = \case
  Codes cs -> Text.intercalate " " $ map printCode cs
  N rn -> "N" <> showText rn
  Assign i e -> "H" <> showText i <> " = " <> printExpr e
  IF (lhs, ord, rhs) (jT, jF) ->
    "IF " <> printExpr lhs <> s <> printExpr rhs
      <> "("
      <> f jT
      <> ","
      <> f jF
      <> ")"
    where
      s = case ord of
        EQ -> "="
        LT -> "<"
        GT -> ">"
      f = maybe "" showText
  JUMP rn -> "JUMP" <> showText rn
  CRT t -> "CRT" <> parens t
  Definiton i _ desc -> "H" <> (showText i) <> "   =  " <> "+000000.0000" <> "  ( " <> Text.justifyLeft 43 ' ' desc <> ")"
  Escape x -> x

parens :: Text -> Text
parens x = "(" <> x <> ")"

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
  | B Expr
  | E Expr
  | I Expr
  deriving (Eq, Show)

printCode :: Code -> Text
printCode = \case
  G x -> "G" <> pad0 2 x
  X x -> "X" <> printExpr x
  Y x -> "Y" <> printExpr x
  Z x -> "Z" <> printExpr x
  U x -> "U" <> printExpr x
  V x -> "V" <> printExpr x
  P x -> "P" <> showText x
  L x -> "L" <> showText x
  C x -> "C" <> pad0 3 x
  M x -> "M" <> pad0 2 x
  Q x -> "Q" <> x
  F x -> "F" <> printExpr x
  B x -> "B" <> printExpr x
  E x -> "E" <> printExpr x
  I x -> "I" <> printExpr x

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

printExpr :: Expr -> Text
printExpr = \case
  Int x -> showText x
  Real x -> showText x
  Var i -> "H" <> showText i
  Neg x -> "-" <> brackets (printExpr x)
  Add x y -> binary "+" x y
  Sub x y -> binary "-" x y
  Mul x y -> binary "*" x y
  Div x y -> binary "/" x y
  Fun f x -> showText f <> brackets (printExpr x)
  where
    binary s x y = brackets $ printExpr x <> s <> printExpr y

brackets :: Text -> Text
brackets x = "[" <> x <> "]"

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
