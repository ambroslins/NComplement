module Generator
  ( Gen,
    Variable (..),
    Name,
    runGenerator,
    emptyEnv,
    throwError,
    showText,
    addVar,
    getVar,
    addLabel,
    getLabel,
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

type Gen = StateT Env (Except Error)

showText :: Show a => a -> Text
showText = Text.pack . show

newtype Address = Address Int
  deriving (Eq)

instance Show Address where
  show (Address x) = Text.unpack $ Text.justifyRight 3 '0' $ showText x

newtype RecordNumber = RecordNumber Int
  deriving (Eq)

instance Show RecordNumber where
  show (RecordNumber x) = Text.unpack $ Text.justifyRight 4 '0' $ showText x

type Name = Text

data Env = Env
  { offsetAddress :: Address,
    recordNumber :: RecordNumber,
    variables :: Map Name Variable,
    labels :: Map Name RecordNumber
  }

data Variable = Variable
  { address :: Address,
    type' :: Type
  }

runGenerator :: Env -> Gen a -> Either Error a
runGenerator env = runExcept . flip evalStateT env

emptyEnv :: Env
emptyEnv =
  Env
    { offsetAddress = Address 0,
      recordNumber = RecordNumber 0,
      variables = Map.empty,
      labels = Map.empty
    }

nextAddress :: Gen Address
nextAddress = do
  Address a <- gets offsetAddress
  modify $ \e -> e {offsetAddress = Address (1 + a)}
  pure $ Address a

nextRecordNumber :: Gen RecordNumber
nextRecordNumber = do
  RecordNumber n <- gets recordNumber
  modify $ \e -> e {recordNumber = RecordNumber (1 + n)}
  pure $ RecordNumber n

addVar :: Name -> Type -> Gen Variable
addVar n t = do
  vars <- gets variables
  case Map.lookup n vars of
    Nothing -> do
      adr <- nextAddress
      let var = Variable {type' = t, address = adr}
      modify (\e -> e {variables = Map.insert n var vars})
      pure var
    Just var -> if t == type' var then pure var else throwError $ Error

getVar :: Name -> Gen Variable
getVar n = gets variables >>= maybe (throwError Error) pure . Map.lookup n

addLabel :: Name -> Gen RecordNumber
addLabel n = do
  ls <- gets labels
  case Map.lookup n ls of
    Nothing -> do
      rn <- nextRecordNumber
      modify $ \e -> e {labels = Map.insert n rn ls}
      pure rn
    Just _ -> throwError $ Error

getLabel :: Name -> Gen RecordNumber
getLabel n = gets labels >>= maybe (throwError Error) pure . Map.lookup n
