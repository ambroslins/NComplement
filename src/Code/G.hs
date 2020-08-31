module Code.G where

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

data G
  = G00 (NonEmpty (Axis, Expr))
  deriving (Eq, Show)

parser :: Parser G
parser = symbol "G00" >> G00 <$> some p
  where
    p = do
      a <- Axis.parser
      e <- Expr.parser
      pure (a, e)

generate :: G -> Gen [Text]
generate (G00 as) = do
  as' <- forM (toList as) $ \(a, expr) -> do
    (t, e) <- Expr.generate expr
    if t == Type.Real
      then pure $ showText a <> e
      else throwError Error
  pure $ pure $ "G00 " <> Text.intercalate " " as'
