module Compile where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Expression (Expression, Name)
import qualified Expression as Expr
import Parse
import Statement (Statement)
import qualified Statement as Stmt
import Text.Megaparsec (eof, errorBundlePretty, parse)
import Type (Type)
import qualified Type

type Compiler = ExceptT Error (State CompilerState)

data Error
  = Error
  | TypeError
  | NotInScope Name
  deriving (Eq, Show)

data CompilerState = CompilerState
  { recordNumber :: Int,
    variables :: Map Name Variable
  }

data Variable = Variable
  { type' :: Type,
    address :: Int
  }

runCompiler :: String -> String
runCompiler s =
  let res =
        fmap compile $
          bimap
            errorBundlePretty
            (fmap concat . mapM compileStatement)
            $ parse (statements <* eof) "NCompiler" s
   in case res of
        Left e -> e
        Right xs -> either show (unlines . map (++ ";")) xs

compile :: Compiler a -> Either Error a
compile = flip evalState start . runExceptT
  where
    start = CompilerState {recordNumber = 0, variables = Map.empty}

compileStatement :: Statement -> Compiler [String]
compileStatement stmt = do
  case stmt of
    Stmt.Assign name expr -> do
      vars <- gets variables
      (t, e) <- compileExpression vars expr
      adr <- case Map.lookup name vars of
        Nothing -> do
          a <- nextAddress
          let var = Variable {type' = t, address = a}
          modify (\s -> s {variables = Map.insert name var vars})
          pure a
        Just var -> do
          if t == type' var then pure $ address var else throwError $ TypeError
      pure $ pure $ 'H' : show adr ++ " = " ++ e
    Stmt.If cond thens elses -> do
      vars <- gets variables
      (t, e) <- compileExpression vars cond
      when (t /= Type.Bool) $ throwError $ Error
      rnElse <- nextRecordNumber
      rnEnd <- nextRecordNumber
      concat
        <$> sequence
          [ pure ["IF " ++ e ++ " (," ++ show rnElse ++ ")"],
            (concat <$> mapM compileStatement thens),
            pure ["JUMP" ++ show rnEnd, "N" ++ show rnElse],
            (concat <$> mapM compileStatement elses),
            pure ["N" ++ show rnEnd]
          ]

compileExpression :: (MonadError Error m) => Map Name Variable -> Expression -> m (Type, String)
compileExpression vars expr = case expr of
  Expr.Lit lit -> pure $ case lit of
    Expr.LitBool x -> (Type.Bool, show x)
    Expr.LitInteger x -> (Type.Integer, show x)
    Expr.LitReal x -> (Type.Real, show x)
  Expr.Var name -> case Map.lookup name vars of
    Nothing -> throwError $ NotInScope name
    Just var -> pure (type' var, 'H' : show (address var))
  Expr.Ref name -> case Map.lookup name vars of
    Nothing -> throwError $ NotInScope name
    Just var -> pure (type' var, show $ address var)
  Expr.Neg x -> do
    (t, e) <- compileExpression vars x
    case t of
      Type.Integer -> pure (Type.Integer, '-' : e)
      Type.Real -> pure (Type.Real, '-' : e)
      _ -> throwError $ TypeError
  Expr.Op op x y -> do
    (tx, ex) <- compileExpression vars x
    (ty, ey) <- compileExpression vars y
    let formatInfix s = "(" ++ ex ++ s ++ ey ++ ")"
    let additive s = do
          t <- case (tx, ty) of
            (Type.Integer, Type.Integer) -> pure Type.Integer
            (Type.Real, Type.Real) -> pure Type.Real
            (_, _) -> throwError $ Error
          pure (t, formatInfix s)
    let multiplicative s c = case (tx, ty) of
          (Type.Integer, Type.Integer) -> pure (Type.Integer, formatInfix s)
          (Type.Integer, Type.Real) -> pure (Type.Real, formatInfix s)
          (Type.Real, Type.Integer) -> pure (Type.Real, formatInfix s)
          (Type.Real, Type.Real) -> pure (Type.Real, "(" ++ ex ++ s ++ ey ++ c ++ "1.)")
          (_, _) -> throwError $ Error
    let comparative s = do
          t <- case (tx, ty) of
            (Type.Integer, Type.Integer) -> pure Type.Bool
            (Type.Real, Type.Real) -> pure Type.Bool
            _ -> throwError $ Error
          pure (t, formatInfix s)
    case op of
      Expr.Add -> additive "+"
      Expr.Sub -> additive "-"
      Expr.Mul -> multiplicative "*" "/"
      Expr.Div -> multiplicative "/" "*"
      Expr.Eq -> if tx == ty then pure (Type.Bool, formatInfix "=") else throwError $ Error
      Expr.Lt -> comparative "<"
      Expr.Gt -> comparative ">"

nextAddress :: (MonadState CompilerState m) => m Int
nextAddress = (+ 100) <$> gets (Map.size . variables)

nextRecordNumber :: (MonadState CompilerState m) => m Int
nextRecordNumber = do
  cs <- get
  let rn = recordNumber cs
  put (cs {recordNumber = rn + 1})
  pure rn
