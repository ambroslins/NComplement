module Gen
  ( Gen,
    Doc',
    Env (..),
    Symbol (..),
    Variable (..),
    Index (..),
    Location (..),
    runGenerator,
    setSourceLine,
    throwE,
    mapDoc,
    get,
    gets,
    modify,
    modifySymbols,
    emit,
    emitWithFuture,
    nextIndex,
    nextLocation,
  )
where

import Control.Monad.Except
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Bifunctor
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Located
import Prettyprinter
import Result
import Syntax
import Type (Type)

type Gen = WriterT (Result Error (Doc')) (ExceptT Error (Tardis Env Env))

type Doc' = Doc ()

data Symbol
  = Var Variable
  | Loc Location
  | Fun ([Expr] -> Gen (Type, Doc'))

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

runGenerator :: Env -> Gen () -> Either Error (Doc')
runGenerator env =
  join
    . flip evalTardis (undefined, env)
    . runExceptT
    . (fmap toEither <$> execWriterT)
    . revert
  where
    revert = (>> (lift $ lift $ getPast >>= sendPast))

setSourceLine :: Pos -> Gen ()
setSourceLine p = modify $ \env -> env {currentLine = p}

throwE :: CompileError -> Gen a
throwE err = do
  pos <- gets currentLine
  src <- gets source
  let l = Text.lines src !! (unPos pos - 1)
  throwError $ CompileError pos l err

mapDoc :: (Doc' -> Doc') -> Gen a -> Gen a
mapDoc f gen = do
  (a, w) <- lift $ runWriterT gen
  tell $ f <$> w
  pure a

get :: Gen Env
get = gets id

gets :: (Env -> a) -> Gen a
gets = lift . lift . getsPast

modify :: (Env -> Env) -> Gen ()
modify = lift . lift . modifyForwards

modifySymbols :: (Map Name Symbol -> Map Name Symbol) -> Gen ()
modifySymbols f = modify $ \env -> env {symbols = f (symbols env)}

emit :: Doc' -> Gen ()
emit = tell . pure

emitWithFuture :: (Env -> Result CompileError (Doc')) -> Gen ()
emitWithFuture f = do
  pos <- gets currentLine
  src <- gets source
  env <- lift $ lift $ getFuture
  let l = Text.lines src !! (unPos pos - 1)
  tell $ first (CompileError pos l) $ f env

nextIndex :: Gen Index
nextIndex = do
  is <- gets indices
  case is of
    [] -> throwE OutOfIndices
    i : is' -> do
      modify $ \env -> env {indices = is'}
      pure i

nextLocation :: Gen Location
nextLocation = do
  ls <- gets locations
  case ls of
    [] -> throwE OutOfLocations
    l : ls' -> do
      modify $ \env -> env {locations = ls'}
      pure l
