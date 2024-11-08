module Loki.Eval
  ( Env (..),
    Value (..),
    CoValue (..),
    eval,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Loki.Core
import Loki.Prelude

eval :: Env -> Statement Name -> IO ()
eval env stmt = runEff $ runReader env $ evalStatement stmt

evalStatement :: (Reader Env :> es, IOE :> es) => Statement Name -> Eff es ()
evalStatement (Prim name args cont) = do
  args' <- traverse evalProducer args
  cont' <- evalConsumer cont

  case name of
    "add" -> case args' of
      [VInt x, VInt y] -> do
        apply cont' (VInt (x + y))
      _ -> error "invalid arguments"
    _ -> error "unknown primitive"
evalStatement (Switch prod clauses def) = do
  value <- evalProducer prod
  go value clauses
  where
    go _ [] = evalStatement def
    go (VInt m) ((Int n, stmt) : _) | m == n = evalStatement stmt
    go value (_ : rest) = go value rest
evalStatement (Cut (Do label stmt) cont) = do
  covalue <- evalConsumer cont
  local (insertCoValue label covalue) do
    evalStatement stmt
evalStatement (Cut prod cont) = do
  value <- evalProducer prod
  covalue <- evalConsumer cont
  apply covalue value

evalProducer :: (Reader Env :> es) => Producer Name -> Eff es Value
evalProducer (Var name) = do
  env <- ask
  case lookupValue name env of
    Just value -> return value
    Nothing -> error "undefined variable"
evalProducer (Const (Int n)) = return (VInt n)
evalProducer (Do _ _) = error "unexpected statement"

evalConsumer :: (Reader Env :> es) => Consumer Name -> Eff es CoValue
evalConsumer Finish = return CFinish
evalConsumer (Label name) = do
  env <- ask
  case lookupCoValue name env of
    Just value -> return value
    Nothing -> error "undefined label"

apply :: (IOE :> es) => CoValue -> Value -> Eff es ()
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

lookupValue :: Name -> Env -> Maybe Value
lookupValue name = M.lookup name . (.values)

lookupCoValue :: Name -> Env -> Maybe CoValue
lookupCoValue name = M.lookup name . (.covalues)

insertCoValue :: Name -> CoValue -> Env -> Env
insertCoValue name value env = env {covalues = M.insert name value env.covalues}

data Value where
  VInt :: Integer -> Value

deriving stock instance Show Value

data CoValue where
  CFinish :: CoValue

deriving stock instance Show CoValue