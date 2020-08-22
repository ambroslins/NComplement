module TypeChecker where

import AST
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map Name Type

runTypeChecker :: Env -> [Statement] -> Either TypeError Env
runTypeChecker env s = runExcept . flip execStateT env $ mapM_ checkStatement s

checkStatement :: (MonadState Env m, MonadError TypeError m) => Statement -> m ()
checkStatement s = do
  env <- get
  case s of
    Assignment var expr -> do
      t <- typeof env expr
      case Map.lookup var env of
        Nothing -> modify $ Map.insert var t
        Just t' -> when (t /= t') $ throwError TypeMissmatch
    If cond sThen sElse -> do
      t <- typeof env cond
      if t == TypeBool
        then do
          mapM_ (checkStatement) sThen
          mapM_ (checkStatement) sElse
        else throwError TypeMissmatch

checkExpression :: Env -> Expression -> Either TypeError Type
checkExpression env = runExcept . typeof env

typeof :: (MonadError TypeError m) => Env -> Expression -> m Type
typeof env = \case
  Lit l -> pure $ case l of
    LitBool _ -> TypeBool
    LitInteger _ -> TypeInteger
    LitReal _ -> TypeReal
  Var v -> case Map.lookup v env of
    Nothing -> throwError $ NotInScope v
    Just t -> pure t
  Neg x -> do
    t <- typeof env x
    case t of
      TypeInteger -> pure TypeInteger
      TypeReal -> pure TypeReal
      _ -> throwError TypeMissmatch
  Add x y -> additive x y
  Sub x y -> additive x y
  Mul x y -> multiplicative x y
  Div x y -> multiplicative x y
  Eq x y -> do
    tx <- typeof env x
    ty <- typeof env y
    if tx == ty then pure TypeBool else throwError TypeMissmatch
  Lt x y -> comparative x y
  Gt x y -> comparative x y
  where
    additive x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (TypeInteger, TypeInteger) -> pure TypeInteger
        (TypeReal, TypeReal) -> pure TypeReal
        _ -> throwError TypeMissmatch
    multiplicative x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (TypeInteger, TypeInteger) -> pure TypeInteger
        (TypeInteger, TypeReal) -> pure TypeReal
        (TypeReal, TypeInteger) -> pure TypeReal
        (TypeReal, TypeReal) -> pure TypeReal
        _ -> throwError TypeMissmatch
    comparative x y = do
      tx <- typeof env x
      ty <- typeof env y
      case (tx, ty) of
        (TypeInteger, TypeInteger) -> pure TypeBool
        (TypeReal, TypeReal) -> pure TypeBool
        _ -> throwError TypeMissmatch
