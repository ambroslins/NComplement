module Compiler
  ( Compiler,
    Variable (..),
    Error (..),
    runCompiler,
    throwError,
    showText,
    insertVar,
    lookupVar,
    startEnv,
    nextAddress,
    nextRecordNumber,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Type (Type)

type Compiler a = StateT Env (Except Error) a

runCompiler :: Compiler a -> Either Error a
runCompiler = runExcept . flip evalStateT startEnv

showText :: Show a => a -> Text
showText = Text.pack . show

data Env = Env
  { offsetAddress :: Address,
    recordNumber :: RecordNumber,
    variables :: Map Name Variable
  }

newtype Address = Address {unAddress :: Int}
  deriving (Eq)

instance Show Address where
  show = Text.unpack . Text.justifyRight 3 '0' . showText . unAddress

newtype RecordNumber = RecordNumber {unRecordNumber :: Int}
  deriving (Eq)

instance Show RecordNumber where
  show = Text.unpack . Text.justifyRight 4 '0' . showText . unRecordNumber

type Name = Text

data Variable = Variable
  { address :: Address,
    type' :: Type
  }

startEnv :: Env
startEnv =
  Env
    { offsetAddress = Address 0,
      recordNumber = RecordNumber 0,
      variables = Map.empty
    }

nextAddress :: (MonadState Env m) => m Address
nextAddress = do
  adr <- gets offsetAddress
  modify $ \e -> e {offsetAddress = Address $ 1 + unAddress adr}
  pure adr

nextRecordNumber :: (MonadState Env m) => m RecordNumber
nextRecordNumber = do
  n <- gets recordNumber
  modify $ \e -> e {recordNumber = RecordNumber $ 1 + unRecordNumber n}
  pure n

insertVar :: (MonadError Error m, MonadState Env m) => Name -> Type -> m Variable
insertVar n t = do
  vars <- gets variables
  case Map.lookup n vars of
    Nothing -> do
      adr <- nextAddress
      let var = Variable {type' = t, address = adr}
      modify (\e -> e {variables = Map.insert n var vars})
      pure var
    Just var -> if t == type' var then pure var else throwError $ Error

lookupVar :: (MonadError Error m, MonadState Env m) => Name -> m Variable
lookupVar n = do
  mvar <- gets (Map.lookup n . variables)
  maybe (throwError $ NotInScope n) pure mvar
