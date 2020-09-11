module NC where

import Data.Text (Text)
import qualified Data.Text as Text
import Literal (Literal)
import qualified Literal as Lit
import Syntax (Code (..), Name (..), Value (..))

data Statement
  = Codes [Code Expr]
  | N RecordNumber
  | Assign Index Expr
  | IF (Expr, Ordering, Expr) (Maybe RecordNumber, Maybe RecordNumber)
  | JUMP RecordNumber
  | CRT Text
  | Definiton Index (Either Int Double) Name
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
  Definiton i _ desc -> "H" <> (showText i) <> "   =  " <> "+000000.0000" <> "  ( " <> Text.justifyLeft 43 ' ' (unName desc) <> ")"
  Escape x -> x

parens :: Text -> Text
parens x = "(" <> x <> ")"

printCode :: Code Expr -> Text
printCode (Code adr val) = adr <> printValue val

printValue :: Value Expr -> Text
printValue (Val x) = x
printValue (Expr x) = printExpr x

data Expr
  = Int Int
  | Real Double
  | Var Index
  | Ref Index
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Fun Function Expr
  deriving (Eq, Show)

fromLit :: Literal -> Expr
fromLit = \case
  Lit.Real x -> Real x
  Lit.Int x -> Int x
  Lit.Bool x -> Int $ if x then 1 else 0

printExpr :: Expr -> Text
printExpr = \case
  Int x -> showText x
  Real x -> showText x
  Var i -> "H" <> showText i
  Ref i -> showText i
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

newtype Index = Index Int
  deriving (Eq, Ord)

instance Show Index where
  show (Index x) = Text.unpack $ pad0 3 x

newtype RecordNumber = RecordNumber Int
  deriving (Eq, Ord)

instance Show RecordNumber where
  show (RecordNumber x) = Text.unpack $ pad0 4 x
