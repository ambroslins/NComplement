module Generator where

import Control.Monad (when)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Error
import Gen
import qualified Literal as Lit
import Located
import qualified NC
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
      vars <- gets symbols
      if name `Map.member` vars
        then throwE $ AlreadyDefined name
        else do
          i <- nextIndex
          let var = Variable {typeof = argType arg, index = i}
          modifySymbols $ Map.insert name (Var var)
          emit $ NC.Definiton i (argDefault arg) name

defineVars :: Gen ()
defineVars = do
  prev <- gets symbols
  emitsWithFuture $ \env ->
    mapMaybe
      ( \case
          (n, Var var) -> Just $ pure $ def n var
          _ -> Nothing
      )
      (Map.toList (Map.difference (symbols env) prev))
  where
    def name var = NC.Definiton (index var) Nothing name

expr :: Expr -> Gen (Type, NC.Expr)
expr = \case
  Lit x -> pure (Lit.type' x, NC.fromLit x)
  Sym name -> do
    syms <- gets symbols
    case Map.lookup name syms of
      Just (Var var) -> pure (typeof var, NC.Var (index var))
      Just (Loc loc) -> pure (Type.Location, NC.Loc loc)
      Just _ -> throwE $ NotAVariable name
      Nothing -> throwE $ UndefinedSymbol name
  Ref name -> do
    syms <- gets symbols
    case Map.lookup name syms of
      Just (Var var) -> pure (Type.Index, NC.Ref (index var))
      Just _ -> throwE $ NotAVariable name
      Nothing -> throwE $ UndefinedSymbol name
  App name args -> do
    syms <- gets symbols
    case Map.lookup name syms of
      Just (Fun f) -> f args
      Just _ -> throwE $ NotAFunction name
      Nothing -> throwE $ UndefinedSymbol name
  Neg x -> do
    (t, e) <- expr x
    case t of
      Type.Int -> pure (Type.Int, NC.Neg e)
      Type.Real -> pure (Type.Real, NC.Neg e)
      _ -> throwE $ TypeMismatchNeg t
  Add x y -> additive NC.Add x y TypeMismatchAdd
  Sub x y -> additive NC.Sub x y TypeMismatchSub
  Mul x y -> do
    (tx, ex) <- expr x
    (ty, ey) <- expr y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, NC.Mul ex ey)
      (Type.Int, Type.Real) -> pure (Type.Real, NC.Mul ex ey)
      (Type.Real, Type.Int) -> pure (Type.Real, NC.Mul ex ey)
      (Type.Real, Type.Real) -> pure (Type.Real, NC.Div (NC.Mul ex ey) (NC.Real 1.0))
      _ -> throwE $ TypeMismatchMul tx ty
  Div x y -> do
    (tx, ex) <- expr x
    (ty, ey) <- expr y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, NC.Div ex ey)
      (Type.Real, Type.Int) -> pure (Type.Real, NC.Div ex ey)
      (Type.Real, Type.Real) -> pure (Type.Real, NC.Mul (NC.Div ex ey) (NC.Real 1.0))
      _ -> throwE $ TypeMismatchDiv tx ty
  Pow n x ->
    expr $
      ( if n < 0
          then Div (Lit $ Lit.Real 1.0)
          else id
      )
        $ case replicate (abs n) x of
          [] -> Lit $ Lit.Int 1
          x' : xs -> foldr Mul x' xs
  where
    additive f x y e = do
      (tx, ex) <- expr x
      (ty, ey) <- expr y
      t <- case (tx, ty) of
        (Type.Int, Type.Int) -> pure Type.Int
        (Type.Real, Type.Real) -> pure Type.Real
        _ -> throwE $ e tx ty
      pure (t, f ex ey)

functions :: [(Name, [Expr] -> Gen (Type, NC.Expr))]
functions =
  [ included "sin" NC.SIN,
    included "cos" NC.COS,
    included "tan" NC.TAN,
    included "asin" NC.ASIN,
    included "acos" NC.ACOS,
    included "atan" NC.ATAN,
    included "sqrt" NC.SQRT,
    included "round" NC.ROUND,
    ( "norm",
      \args -> case map (Pow 2) args of
        [] -> throwE $ TypeMismatchApp "norm" [Type.Real, Type.Real] []
        x : xs -> expr $ App "sqrt" $ [foldl Add x xs]
    )
  ]
  where
    included name f =
      ( name,
        \args -> do
          xs <- mapM expr args
          case xs of
            [(t, x)] -> case t of
              Type.Real -> pure $ (Type.Real, NC.App f x)
              _ -> throwE $ TypeMismatchApp name [Type.Real] [t]
            xs' -> throwE $ TypeMismatchApp name [Type.Real] (map fst xs')
      )

statement :: Statement -> Gen ()
statement (At pos stmt) = do
  setSourceLine pos
  case stmt of
    Assign name x -> do
      (t, e) <- expr x
      var <- defineVar name t
      emit $ NC.Assign (index var) e
    If (lhs, ord, rhs) thens melses -> do
      (tl, el) <- expr lhs
      (tr, er) <- expr rhs
      when (tl /= tr) $ throwE $ TypeMismatchIf tl tr
      l1 <- nextLocation
      emit $ NC.IF (el, ord, er) (Nothing, Just l1)
      statement thens
      case melses of
        Nothing -> emit $ NC.Codes [NC.n l1]
        Just elses -> do
          l2 <- nextLocation
          emits $ NC.Codes . pure <$> [NC.jump l2, NC.n l1]
          statement elses
          emit $ NC.Codes [NC.n l2]
    Scope stmts -> mapM_ statement stmts
    Unsafe x -> do
      xs <- mapM (either pure (fmap (NC.toText . snd) . expr)) x
      emit $ NC.Escape $ Text.concat xs
    Label name -> do
      syms <- gets symbols
      if name `Map.member` syms
        then throwE $ AlreadyDefined name
        else do
          loc <- nextLocation
          modifySymbols $ Map.insert name (Loc loc)
          emit $ NC.Codes [NC.n loc, NC.Comment $ unName name]
    Codes cs -> emit =<< NC.Codes <$> instr (toList cs)
    Get names address -> do
      xs <- f (toList names) (toList address)
      emit $ NC.Codes (NC.g 83 : xs)
      where
        f [] [] = pure []
        f (n : ns) (a : as) = case Map.lookup a getters of
          Nothing -> throwE $ NotAGetter a
          Just t -> do
            var <- defineVar n t
            (NC.Code a (NC.Ref (index var)) :) <$> f ns as
        f _ _ = throwE GetSetNotMatching
        getters =
          Map.fromList $
            [ ("X", Type.Real),
              ("Y", Type.Real),
              ("Z", Type.Real),
              ("U", Type.Real),
              ("V", Type.Real),
              ("I", Type.Int),
              ("E", Type.Int)
            ]
    Set address exprs -> do
      xs <- f (toList address) (toList exprs)
      emit $ NC.Codes (NC.g 92 : xs)
      where
        f [] [] = pure []
        f (a : as) (e : es) = case Map.lookup a setters of
          Nothing -> throwE $ NotASetter a
          Just t -> do
            (t', e') <- expr e
            if t == t'
              then (NC.Code a e' :) <$> f as es
              else throwE $ TypeMismatchSet a t t'
        f _ _ = throwE GetSetNotMatching
        setters =
          Map.fromList $
            [ ("X", Type.Real),
              ("Y", Type.Real),
              ("Z", Type.Real),
              ("U", Type.Real),
              ("V", Type.Real)
            ]
  where
    defineVar name t = do
      syms <- gets symbols
      case Map.lookup name syms of
        Nothing -> do
          i <- nextIndex
          let var = Variable {typeof = t, index = i}
          modifySymbols $ Map.insert name (Var var)
          pure var
        Just (Var var) -> do
          let t' = typeof var
          if t' == t
            then pure var
            else throwE $ TypeMismatchDef name t' t
        Just _ -> throwE $ NotAVariable name

instr :: [Code] -> Gen [NC.Code]
instr = mapM $ \(Code adr x) -> NC.Code adr . snd <$> expr x
