module NC where

import Data.Text (Text)
import qualified Data.Text as Text
import Literal (Literal)
import qualified Literal as Lit
import Numeric
import Prettyprinter
import Prettyprinter.Render.Text
import Syntax
  ( Address (..),
    Index (..),
    Location (..),
    Sign (..),
  )

digit :: Int
digit = 1

data Statement
  = Codes [Code]
  | Assign Index Expr
  | IF (Expr, Ordering, Expr) (Maybe Location, Maybe Location)
  | Call Address [Expr]
  | Definiton Index (Maybe (Sign, Literal)) Text
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
g = Code "G" . Int

n :: Location -> Code
n = Code "N" . Loc

jump :: Location -> Code
jump = Code "JUMP" . Loc

render :: [Statement] -> Text
render =
  renderStrict
    . layoutPretty options
    . vsep
    . map (<> ";")
    . map pretty
  where
    options = LayoutOptions Unbounded

instance Pretty Statement where
  pretty = \case
    Codes cs -> hsep $ map pretty cs
    Assign i x -> "H" <> prettyIndex i <+> "=" <+> pretty x
    IF (lhs, ord, rhs) (jT, jF) ->
      "IF" <+> pretty lhs <> s <> pretty rhs
        <+> tupled (maybe "" prettyLocation <$> [jT, jF])
      where
        s = case ord of
          EQ -> "="
          LT -> "<"
          GT -> ">"
    Call adr xs -> prettyAddress adr <> tupled (map pretty xs)
    Definiton i def desc ->
      "H" <> prettyIndex i <> "   = " <> val def
        <> "   "
        <> parens
          (column $ \c -> pretty $ Text.justifyLeft (70 - c) ' ' (Text.toUpper desc))
      where
        val = \case
          Just (s, Lit.Real x) ->
            let (int, frac) = Text.breakOn "." (Text.pack $ showFFloatAlt Nothing (abs x) "")
             in pretty $
                  sign s <> Text.justifyRight (7 - digit) '0' int
                    <> Text.take
                      (4 + digit)
                      (Text.justifyLeft (4 + digit) '0' frac)
          Just (s, Lit.Int x) ->
            (pretty $ sign s <> Text.replicate (7 - digit) "0" <> ".")
              <> pad0 (3 + digit) (abs x)
          Just (_, Lit.Bool x) ->
            val $ Just (Plus, Lit.Int $ if x then 1 else 0)
          _ -> val $ Just (Plus, Lit.Int 0)
        sign Minus = "-"
        sign Plus = "+"
    Escape x -> pretty x

instance Pretty Code where
  pretty = \case
    Code "G" (Int x) -> "G" <> pad0 2 x
    Code "M" (Int x) -> "M" <> pad0 2 x
    Code "T" (Int x) -> "T" <> pad0 3 x
    Code "C" (Int x) -> "C" <> pad0 3 x
    Code adr x -> prettyAddress adr <> pretty x
    Comment x -> parens $ pretty (Text.toUpper x)

instance Pretty Expr where
  pretty = \case
    Int x -> viaShow x
    Real x ->
      let (int, frac) = Text.breakOn ". " $ Text.pack $ showFFloatAlt Nothing x ""
       in pretty $ int <> Text.take 4 frac
    Var i -> "H" <> prettyIndex i
    Ref i -> prettyIndex i
    Loc l -> prettyLocation l
    String t -> pretty t
    Neg x -> "-" <> brackets (pretty x)
    Add x y -> binary "+" x y
    Sub x y -> binary "-" x y
    Mul x y -> binary "*" x y
    Div x y -> binary "/" x y
    App f x -> viaShow f <> brackets (pretty x)
    where
      binary s x y = brackets $ pretty x <> s <> pretty y

prettyIndex :: Index -> Doc ann
prettyIndex = \case
  Index x -> pad0 3 x
  Return -> "RET"

prettyLocation :: Location -> Doc ann
prettyLocation (Location x) = pad0 4 x

prettyAddress :: Address -> Doc ann
prettyAddress (Address x) = pretty x

pad0 :: Show a => Int -> a -> Doc ann
pad0 x = pretty . Text.justifyRight x '0' . Text.pack . show
