module Code where

import Axis (Axis)
import qualified Axis
import Control.Monad (forM)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text
import Error
import Expression (Expr)
import qualified Expression as Expr
import Generator
import Parser
import qualified Type

data Code
  = G00 (NonEmpty (Axis, Expr))
  deriving (Eq, Show)

parser :: Parser Code
parser = symbol "G00" >> G00 <$> some p
  where
    p = do
      a <- Axis.parser
      e <- Expr.parser
      pure (a, e)

generate :: Code -> Gen ()
generate (G00 as) = do
  as' <- forM (toList as) $ \(a, expr) -> do
    (t, e) <- Expr.eval expr
    if t == Type.Real
      then pure $ showText a <> e
      else throwError Error
  emit $ "G00 " <> Text.intercalate " " as'
