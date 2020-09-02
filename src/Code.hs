module Code where

import Axis (Axis)
import qualified Axis
import Control.Monad (forM, unless)
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
  | G01 (NonEmpty (Axis, Expr))
  | G04 Expr
  | G05
  | G06
  | G07
  | G08
  | G09
  | G11
  | G12
  | Coord Expr
  | G80 (NonEmpty (Axis, Expr))
  deriving (Eq, Show)

parser :: Parser Code
parser =
  choice
    [ choice
        [ G00 <$ symbol "G00",
          G01 <$ symbol "G01",
          G80 <$ symbol "G80"
        ]
        <*> some p,
      G04 <$ symbol "G04" <*> (symbol "X" *> Expr.parser),
      G05 <$ symbol "G05",
      G06 <$ symbol "G06",
      G07 <$ symbol "G07",
      G08 <$ symbol "G08",
      G09 <$ symbol "G09",
      G11 <$ symbol "G11",
      G12 <$ symbol "G12",
      Coord <$> (symbol "G" *> Expr.parser)
    ]
  where
    p = do
      a <- Axis.parser
      e <- Expr.parser
      pure (a, e)

generate :: Code -> Gen ()
generate = \case
  G00 as -> do
    as' <- forM (toList as) $ \(a, expr) -> do
      (t, e) <- Expr.eval expr
      unless (t == Type.Real) $ throwError $ TypeMismatch t Type.Real
      pure $ showText a <> e
    emit $ "G00 " <> Text.intercalate " " as'
  G01 as -> do
    as' <- forM (toList as) $ \(a, expr) -> do
      (t, e) <- Expr.eval expr
      unless (t == Type.Real) $ throwError $ TypeMismatch t Type.Real
      pure $ showText a <> e
    emit $ "G01 " <> Text.intercalate " " as'
  G04 s -> do
    (t, e) <- Expr.eval s
    unless (t `elem` [Type.Real, Type.Int]) $ throwError $ TypeMismatch t Type.Real
    emit $ "G04 X" <> e
  G05 -> emit "G05"
  G06 -> emit "G06"
  G07 -> emit "G07"
  G08 -> emit "G08"
  G09 -> emit "G09"
  G11 -> emit "G11"
  G12 -> emit "G12"
  Coord expr -> do
    (t, x) <- Expr.eval expr
    unless (t == Type.Int) $ throwError $ TypeMismatch t Type.Int
    emit $ "G" <> x
  G80 as -> do
    as' <- forM (toList as) $ \(a, expr) -> do
      (t, e) <- Expr.eval expr
      unless (t == Type.Real) $ throwError $ TypeMismatch t Type.Real
      pure $ showText a <> e
    emit $ "G80 " <> Text.intercalate " " as'
