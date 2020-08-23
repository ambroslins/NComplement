module Expression
  ( Name,
    Expression (..),
    Operator (..),
    Literal (..),
    typeOf,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Type (Type (..))
import qualified Type

type Name = String

data Expression
  = Lit Literal
  | Var Name
  | Ref Name
  | Neg Expression
  | Op Operator Expression Expression
  deriving (Eq, Show)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Gt
  deriving (Eq, Show)

data Literal
  = LitBool Bool
  | LitInteger Integer
  | LitReal Double
  deriving (Eq)

instance Show Literal where
  show = \case
    LitBool x -> show x
    LitInteger x -> show x
    LitReal x -> show x

typeOf :: (MonadError Type.Error m) => Map Name Type -> Expression -> m Type
typeOf env expr = case expr of
  Lit lit -> pure $ case lit of
    LitBool _ -> Type.Bool
    LitInteger _ -> Type.Integer
    LitReal _ -> Type.Real
  Var name -> case Map.lookup name env of
    Nothing -> throwError $ Type.NotInScope name
    Just t -> pure t
  Ref name -> case Map.lookup name env of
    Nothing -> throwError $ Type.NotInScope name
    Just t -> pure t
  Neg x -> do
    t <- typeOf env x
    case t of
      Integer -> pure Type.Integer
      Real -> pure Type.Real
      _ -> throwError $ Type.Error
  Op op x y -> do
    tx <- typeOf env x
    ty <- typeOf env y
    let additive = case (tx, ty) of
          (Type.Integer, Type.Integer) -> pure Type.Integer
          (Type.Real, Type.Real) -> pure Type.Real
          (_, _) -> throwError $ Type.Error
    let multiplicative = case (tx, ty) of
          (Type.Integer, Type.Integer) -> pure Type.Integer
          (Type.Integer, Type.Real) -> pure Type.Real
          (Type.Real, Type.Integer) -> pure Type.Real
          (Type.Real, Type.Real) -> pure Type.Real
          (_, _) -> throwError $ Type.Error
    let comparative = case (tx, ty) of
          (Type.Integer, Type.Integer) -> pure Type.Bool
          (Type.Real, Type.Real) -> pure Type.Bool
          _ -> throwError $ Type.Error
    case op of
      Add -> additive
      Sub -> additive
      Mul -> multiplicative
      Div -> multiplicative
      Eq -> if tx == ty then pure Type.Bool else throwError $ Type.Error
      Lt -> comparative
      Gt -> comparative
