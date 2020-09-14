module Gen
  ( Gen,
    Env (..),
    Symbol (..),
    Variable (..),
    Index (..),
    Location (..),
    runGenerator,
    setSourceLine,
    throwE,
    get,
    gets,
    modify,
    modifySymbols,
    emit,
    emits,
    emitWithFuture,
    emitsWithFuture,
    nextIndex,
    nextLocation,
  )
where

import Control.Monad.Except
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Located
import qualified NC
import Syntax
import Type (Type)

type Gen = WriterT [Either Error NC.Statement] (ExceptT Error (Tardis Env Env))

data Symbol
  = Var Variable
  | Loc Location
  | Fun ([Expr] -> Gen (Type, NC.Expr))

data Variable = Variable
  { typeof :: Type,
    index :: Index
  }

data Env = Env
  { indices :: [Index],
    locations :: [Location],
    source :: Text,
    currentLine :: Pos,
    symbols :: Map Name Symbol
  }

runGenerator :: Env -> Gen () -> Either Error [NC.Statement]
runGenerator env = join . fmap foldEither . flip evalTardis (undefined, env) . runExceptT . execWriterT . revert
  where
    foldEither [] = Right []
    foldEither (Left x : _) = Left x
    foldEither (Right x : xs) = (x :) <$> foldEither xs
    revert = (>> (lift $ lift $ getPast >>= sendPast))

setSourceLine :: Pos -> Gen ()
setSourceLine p = modify $ \env -> env {currentLine = p}

throwE :: Error' -> Gen a
throwE err = do
  pos <- gets currentLine
  src <- gets source
  let line = Text.lines src !! (unPos pos - 1)
  throwError $ CompileError pos line err

get :: Gen Env
get = gets id

gets :: (Env -> a) -> Gen a
gets = lift . lift . getsPast

modify :: (Env -> Env) -> Gen ()
modify = lift . lift . modifyForwards

modifySymbols :: (Map Name Symbol -> Map Name Symbol) -> Gen ()
modifySymbols f = modify $ \env -> env {symbols = f (symbols env)}

emit :: NC.Statement -> Gen ()
emit = tell . pure . pure

emits :: [NC.Statement] -> Gen ()
emits = mapM_ emit

emitWithFuture :: (Env -> Either Error NC.Statement) -> Gen ()
emitWithFuture = emitsWithFuture . fmap pure

emitsWithFuture :: (Env -> [Either Error NC.Statement]) -> Gen ()
emitsWithFuture f = (lift $ lift $ getFuture) >>= tell . f

nextIndex :: Gen Index
nextIndex = do
  is <- gets indices
  case is of
    [] -> throwE Error
    i : is' -> do
      modify $ \env -> env {indices = is'}
      pure i

nextLocation :: Gen Location
nextLocation = do
  ls <- gets locations
  case ls of
    [] -> throwE Error
    l : ls' -> do
      modify $ \env -> env {locations = ls'}
      pure l
