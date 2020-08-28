module Code.G where

import Axis (Axis)
import qualified Axis
import Compiler
import Control.Monad (forM)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text
import Expression (Expr)
import qualified Expression as Expr
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

compile :: G -> Compiler [Text]
compile (G00 as) = do
  as' <- forM (toList as) $ \(a, expr) -> do
    (t, e) <- Expr.compile expr
    if t == Type.Real
      then pure $ showText a <> e
      else throwError Error
  pure $ pure $ "G00 " <> Text.intercalate " " as'
