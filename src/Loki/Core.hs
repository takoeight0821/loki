{-# LANGUAGE NoMonomorphismRestriction #-}

module Loki.Core
  ( Producer (..),
    Consumer (..),
    Statement (..),
    prim,
    switch,
    Const (..),
    Name (..),
    Unique (..),
    newName,
    ex1,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.State.Static.Shared (State, evalState, get, put)
import Loki.Prelude

data Producer a where
  Var :: a -> Producer a
  Const :: Const -> Producer a
  Do :: a -> Statement a -> Producer a

deriving stock instance (Show a) => Show (Producer a)

data Consumer a where
  Finish :: Consumer a
  Label :: a -> Consumer a

deriving stock instance (Show a) => Show (Consumer a)

data Statement a where
  Prim :: Text -> [Producer a] -> Consumer a -> Statement a
  Switch :: Producer a -> [(Const, Statement a)] -> Statement a -> Statement a
  Cut :: Producer a -> Consumer a -> Statement a

deriving stock instance (Show a) => Show (Statement a)

prim :: (State Unique :> es) => Text -> [Producer Name] -> Eff es (Producer Name)
prim name args = do
  label <- newName name
  return $ Do label (Prim name args (Label label))

switch :: (State Unique :> es) => Producer Name -> [(Const, Producer Name)] -> Producer Name -> Eff es (Producer Name)
switch expr clauses def = do
  label <- newName "switch"
  return
    $ Do label
    $ Switch expr (map (\(c, p) -> (c, Cut p (Label label))) clauses)
    $ Cut def (Label label)

data Const where
  Int :: Integer -> Const

deriving stock instance Show Const

data Name = Name
  { name :: Text,
    unique :: Unique
  }
  deriving stock (Eq, Ord)

instance Show Name where
  show Name {..} = convertString name <> "." <> show unique

newtype Unique = Unique {unUnique :: Int}
  deriving newtype (Show, Eq, Ord)

newName :: (State Unique :> es) => Text -> Eff es Name
newName name = do
  unique <- get
  put $ Unique $ unique.unUnique + 1
  return $ Name {..}

ex1 :: Producer Name
ex1 =
  runPureEff
    $ evalState (Unique 0)
    $ prim "add" [Const (Int 1), Const (Int 2)]