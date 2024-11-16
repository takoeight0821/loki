module Loki.Eval
  ( Env (..),
    Value (..),
    CoValue (..),
    eval,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Text (Text)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Static (CallStack, Error, HasCallStack, prettyCallStack, runErrorWith, throwError)
import Effectful.FileSystem.IO (stderr)
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Loki.Core
import Loki.Prelude
import System.IO (hPutStrLn)

eval :: (HasCallStack) => Env -> Statement Name -> IO ()
eval env stmt = runEff $ runReader env $ runErrorWith handler $ evalStatement stmt
  where
    handler :: (IOE :> es) => CallStack -> EvalError -> Eff es ()
    handler stack err = liftIO $ do
      hPutStrLn stderr $ "Error: " <> show err
      hPutStrLn stderr $ prettyCallStack stack

data EvalError
  = InvalidArguments Location [Value]
  | UnknownPrimitive Location Text
  | UndefinedVariable Location Name
  | UndefinedLabel Location Name
  | UnexpectedProducer Location (Producer Name)
  deriving stock (Show)

evalStatement :: (HasCallStack, Reader Env :> es, IOE :> es, Error EvalError :> es) => Statement Name -> Eff es ()
evalStatement (Prim loc name args cont) = do
  args' <- traverse evalProducer args
  cont' <- evalConsumer cont

  case name of
    "add" -> case args' of
      [VInt x, VInt y] -> do
        apply cont' (VInt (x + y))
      _ -> throwError $ InvalidArguments loc args'
    _ -> throwError $ UnknownPrimitive loc name
evalStatement (Switch _ prod clauses def) = do
  value <- evalProducer prod
  go value clauses
  where
    go _ [] = evalStatement def
    go (VInt m) ((Int n, stmt) : _) | m == n = evalStatement stmt
    go value (_ : rest) = go value rest
evalStatement (Cut _ (Do _ label stmt) cont) = do
  covalue <- evalConsumer cont
  local (insertCoValue label covalue) do
    evalStatement stmt
evalStatement (Cut _ prod cont) = do
  value <- evalProducer prod
  covalue <- evalConsumer cont
  apply covalue value

evalProducer :: (HasCallStack, Reader Env :> es, Error EvalError :> es) => Producer Name -> Eff es Value
evalProducer (Var loc name) = lookupValue loc name
evalProducer (Const _ (Int n)) = return (VInt n)
evalProducer p = throwError $ UnexpectedProducer (location p) p

evalConsumer :: (HasCallStack, Reader Env :> es, Error EvalError :> es) => Consumer Name -> Eff es CoValue
evalConsumer (Finish _) = return CFinish
evalConsumer (Label loc name) = lookupCoValue loc name

apply :: (HasCallStack, IOE :> es) => CoValue -> Value -> Eff es ()
apply CFinish x = liftIO $ print x

data Env = Env
  { values :: Map Name Value,
    covalues :: Map Name CoValue
  }
  deriving stock (Show)

instance Semigroup Env where
  Env v1 c1 <> Env v2 c2 = Env (v1 <> v2) (c1 <> c2)

instance Monoid Env where
  mempty = Env mempty mempty

lookupValue :: (HasCallStack, Reader Env :> es, Error EvalError :> es) => Location -> Name -> Eff es Value
lookupValue loc name = do
  env <- ask @Env
  case M.lookup name env.values of
    Just value -> return value
    Nothing -> throwError $ UndefinedVariable loc name

lookupCoValue :: (HasCallStack, Reader Env :> es, Error EvalError :> es) => Location -> Name -> Eff es CoValue
lookupCoValue loc name = do
  env <- ask @Env
  case M.lookup name env.covalues of
    Just value -> return value
    Nothing -> throwError $ UndefinedLabel loc name

insertCoValue :: Name -> CoValue -> Env -> Env
insertCoValue name value env = env {covalues = M.insert name value env.covalues}

data Value where
  VInt :: Integer -> Value

deriving stock instance Show Value

data CoValue where
  CFinish :: CoValue

deriving stock instance Show CoValue