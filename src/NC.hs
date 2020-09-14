module NC where

import Data.Text (Text)
import qualified Data.Text as Text
import Literal (Literal)
import qualified Literal as Lit
import Syntax
  ( Address (..),
    Index (..),
    Location (..),
    Name (..),
  )

data Statement
  = Codes [Code]
  | Assign Index Expr
  | IF (Expr, Ordering, Expr) (Maybe Location, Maybe Location)
  | CRT Text
  | Definiton Index (Either Int Double) Name
  | Escape Text
  deriving (Eq, Show)

data Code
  = Code Address Expr
  | Comment Text
  deriving (Eq, Show)

data Expr
  = Int Int
  | Real Double
  | Var Index
  | Ref Index
  | Loc Location
  | String Text
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | App Function Expr
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

fromLit :: Literal -> Expr
fromLit = \case
  Lit.Real x -> Real x
  Lit.Int x -> Int x
  Lit.Bool x -> Int $ if x then 1 else 0
  Lit.String x -> String x

g :: Int -> Code
g = Code "G" . (String . pad0 2)

n :: Location -> Code
n = Code "N" . Loc

jump :: Location -> Code
jump = Code "JUMP" . Loc

class ToText a where
  toText :: a -> Text

printStmts :: [Statement] -> Text
printStmts = Text.unlines . map (<> ";") . map toText

instance ToText Statement where
  toText = \case
    Codes cs -> Text.intercalate " " $ map toText cs
    Assign (Index i) e -> "H" <> pad0 3 i <> " = " <> toText e
    IF (lhs, ord, rhs) (jT, jF) ->
      "IF " <> toText lhs <> s <> toText rhs
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
        f = maybe "" toText
    CRT t -> "CRT" <> parens t
    Definiton i _ desc -> "H" <> (toText i) <> "   =  " <> "+000000.0000" <> "  ( " <> Text.justifyLeft 43 ' ' (unName desc) <> ")"
    Escape x -> x

instance ToText Code where
  toText = \case
    (Code adr x) -> toText adr <> toText x
    Comment x -> parens (Text.toUpper x)

instance ToText Expr where
  toText = \case
    Int x -> Text.pack $ show x
    Real x -> Text.pack $ show x
    Var i -> "H" <> toText i
    Ref i -> toText i
    Loc l -> toText l
    String t -> t
    Neg x -> "-" <> brackets (toText x)
    Add x y -> binary "+" x y
    Sub x y -> binary "-" x y
    Mul x y -> binary "*" x y
    Div x y -> binary "/" x y
    App f x -> Text.pack (show f) <> brackets (toText x)
    where
      binary s x y = brackets $ toText x <> s <> toText y

instance ToText Index where
  toText (Index x) = pad0 3 x

instance ToText Location where
  toText (Location x) = pad0 4 x

instance ToText Address where
  toText (Address x) = x

parens :: Text -> Text
parens x = "(" <> x <> ")"

brackets :: Text -> Text
brackets x = "[" <> x <> "]"

pad0 :: Show a => Int -> a -> Text
pad0 x = Text.justifyRight x '0' . Text.pack . show
