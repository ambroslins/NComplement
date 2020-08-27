module Code where

import Compiler
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Expression (Expr)
import qualified Expression as Expr
import Parser
import qualified Type

data Code
  = G00 (NonEmpty (Axis, Expr))
  deriving (Eq, Show)

data Axis = X | Y | Z | U | V
  deriving (Eq, Show)

axis :: Parser Axis
axis =
  choice
    [ X <$ symbol "X",
      Y <$ symbol "Y",
      Z <$ symbol "Z",
      U <$ symbol "U",
      V <$ symbol "V"
    ]

parser :: Parser Code
parser = G00 <$ symbol "G00" <*> some p
  where
    p = do
      a <- axis
      e <- Expr.parser
      pure (a, e)

compile :: Code -> Compiler [Text]
compile c = case c of
  G00 xs -> do
    let f (a, expr) = do
          (t, e) <- Expr.compile expr
          if t == Type.Real then pure $ showText a <> e else throwError $ Error
    as <- mapM f $ toList xs
    pure $ pure $ "G00 " <> Text.intercalate " " as
