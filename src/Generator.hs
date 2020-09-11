module Generator where

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Error
import Gen
import qualified Literal as Lit
import qualified NC
import qualified Parser
import Replace.Megaparsec (splitCap)
import Syntax
import Type (Type)
import qualified Type

program :: Program -> Gen ()
program p = do
  defineArgs (arguments p)
  defineVars
  mapM_ statement (body p)

defineArgs :: [(Name, Argument)] -> Gen ()
defineArgs = mapM_ def
  where
    def (name, arg) = do
      vars <- gets variables
      if name `Map.member` vars
        then throwError $ Error
        else do
          i <- nextIndex
          let var = Variable {type' = argType arg, index = i}
          modifyVars $ Map.insert name var
          emit $ NC.Definiton i (Left 0) name

defineVars :: Gen ()
defineVars = do
  prev <- gets variables
  emitsWithFuture $ \env ->
    map
      (pure . def)
      (Map.toList (Map.difference (variables env) prev))
  where
    def (name, var) = NC.Definiton (index var) (Left 0) name

expr :: Expr -> Gen (Type, NC.Expr)
expr = \case
  Lit x -> pure (Lit.type' x, Lit.toNC x)
  Var name -> do
    var <- gets variables >>= maybe (throwError $ UndefinedVar name) pure . Map.lookup name
    pure (type' var, NC.Var (index var))
  Ref name -> do
    var <- gets variables >>= maybe (throwError $ UndefinedVar name) pure . Map.lookup name
    pure (type' var, NC.Int (unIndex $ index var))
  Fun name args ->
    maybe
      (throwError $ UndefinedFun name)
      ($ args)
      $ lookup name functions
  Neg x -> do
    (t, e) <- expr x
    case t of
      Type.Int -> pure (Type.Int, NC.Neg e)
      Type.Real -> pure (Type.Real, NC.Neg e)
      _ -> throwError $ TypeMismatch t Type.Real
  Add x y -> additive NC.Add x y
  Sub x y -> additive NC.Sub x y
  Mul x y -> do
    (tx, ex) <- expr x
    (ty, ey) <- expr y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, NC.Mul ex ey)
      (Type.Int, Type.Real) -> pure (Type.Real, NC.Mul ex ey)
      (Type.Real, Type.Int) -> pure (Type.Real, NC.Mul ex ey)
      (Type.Real, Type.Real) -> pure (Type.Real, NC.Div (NC.Mul ex ey) (NC.Real 1.0))
      _ -> throwError $ TypeMismatch tx ty
  Div x y -> do
    (tx, ex) <- expr x
    (ty, ey) <- expr y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, NC.Div ex ey)
      (Type.Real, Type.Int) -> pure (Type.Real, NC.Div ex ey)
      (Type.Real, Type.Real) -> pure (Type.Real, NC.Mul (NC.Div ex ey) (NC.Real 1.0))
      _ -> throwError $ TypeMismatch tx ty
  Pow n e ->
    expr $
      ( if n < 0
          then Div (Lit $ Lit.Real 1.0)
          else id
      )
        $ case replicate (abs n) e of
          [] -> Lit $ Lit.Int 1
          x : xs -> foldr Mul x xs
  where
    additive f x y = do
      (tx, ex) <- expr x
      (ty, ey) <- expr y
      t <- case (tx, ty) of
        (Type.Int, Type.Int) -> pure Type.Int
        (Type.Real, Type.Real) -> pure Type.Real
        _ -> throwError $ TypeMismatch tx ty
      pure (t, f ex ey)

functions :: [(Name, [Expr] -> Gen (Type, NC.Expr))]
functions =
  [ ("sin", included NC.SIN),
    ("cos", included NC.COS),
    ("tan", included NC.TAN),
    ("asin", included NC.ASIN),
    ("acos", included NC.ACOS),
    ("atan", included NC.ATAN),
    ("sqrt", included NC.SQRT),
    ("round", included NC.ROUND),
    ( "norm",
      \xs -> case map (Pow 2) xs of
        [] -> throwError $ Error
        x : xs' -> expr $ Fun "sqrt" $ [foldl Add x xs']
    )
  ]
  where
    included f = \case
      [x] -> do
        (t, e) <- expr x
        case t of
          Type.Real -> pure $ (Type.Real, NC.Fun f e)
          _ -> throwError $ TypeMismatch t Type.Real
      _ -> throwError $ Error

statement :: Statement -> Gen ()
statement stmt = do
  case stmt of
    Assign name x -> do
      (t, e) <- expr x
      var <- do
        vars <- gets variables
        case Map.lookup name vars of
          Nothing -> do
            i <- nextIndex
            let v = Variable {index = i, type' = t}
            modifyVars $ Map.insert name v
            pure v
          Just v -> do
            when (type' v /= t) $ throwError $ Error
            pure v
      emit $ NC.Assign (index var) e
    If (lhs, ord, rhs) thens melses -> do
      (tl, el) <- expr lhs
      (tr, er) <- expr rhs
      when (tl /= tr) $ throwError $ Error
      rn1 <- nextRecordNumber
      emit $ NC.IF (el, ord, er) (Nothing, Just rn1)
      statement thens
      case melses of
        Nothing -> emit $ NC.N rn1
        Just elses -> do
          rn2 <- nextRecordNumber
          emits $ [NC.JUMP rn2, NC.N rn1]
          statement elses
          emit $ NC.N rn2
    Scope stmts -> mapM_ statement stmts
    Unsafe x ->
      emit
        =<< ( fmap (NC.Escape . Text.concat) $
                mapM (either pure (fmap (Text.pack . show . snd) . expr)) $
                  splitCap (Parser.reference <|> Parser.variable) x
            )
    Label name -> do
      ls <- gets labels
      when (Map.member name ls) $ throwError $ Error
      rn <- nextRecordNumber
      modifyLabels $ Map.insert name rn
      emit $ NC.N rn
    Jump name ->
      emitWithFuture $
        maybe
          (Left $ UndefinedLabel name)
          (Right . NC.JUMP)
          . Map.lookup name
          . labels
    Codes cs -> emit =<< NC.Codes <$> instr (toList cs)

instr :: [Code] -> Gen [NC.Code]
instr = \case
  [] -> pure []
  G 0 : xs -> do
    xs' <- forM xs $ \case
      M 5 -> pure $ NC.M 5
      F f -> NC.F <$> requireType Type.Real f
      x -> axis x
    pure $ NC.G 0 : xs'
  G 1 : xs -> do
    xs' <- forM xs $ \case
      F x -> NC.F <$> requireType Type.Real x
      x -> axis x
    pure $ NC.G 1 : xs'
  G 4 : xs -> case xs of
    [X x] -> sequence $ [pure $ NC.G 4, NC.X <$> requireType Type.Real x]
    _ -> throwError $ Error
  G 80 : xs -> forM xs axis
  G 82 : xs -> forM xs axis
  G 83 : xs -> forM xs $ \case
    X x -> NC.X <$> requireType Type.Int x
    Y x -> NC.Y <$> requireType Type.Int x
    Z x -> NC.Z <$> requireType Type.Int x
    U x -> NC.U <$> requireType Type.Int x
    V x -> NC.V <$> requireType Type.Int x
    B x -> NC.B <$> requireType Type.Int x
    E x -> NC.E <$> requireType Type.Int x
    I x -> NC.I <$> requireType Type.Int x
    _ -> throwError $ Error
  G g : cs | Set.member g modalG -> (NC.G g :) <$> (instr cs)
  _ -> throwError Error
  where
    axis = \case
      X x -> NC.X <$> requireType Type.Real x
      Y x -> NC.Y <$> requireType Type.Real x
      Z x -> NC.Z <$> requireType Type.Real x
      U x -> NC.U <$> requireType Type.Real x
      V x -> NC.U <$> requireType Type.Real x
      _ -> throwError Error
    requireType t x = do
      (t', x') <- expr x
      if t' == t then pure x' else throwError $ TypeMismatch t' t
    modalG =
      Set.fromList $
        concat
          [ [5 .. 9],
            [11, 12],
            [26, 27],
            [28 .. 30],
            [40 .. 42],
            [100 * x + y | x <- [0 .. 9], y <- [54 .. 59]],
            [90, 91],
            [126, 127]
          ]
