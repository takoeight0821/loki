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
    Location (..),
    HasLocation (..),
    locCS,
    LocationRepr (..),
    ex1,
    ex2,
  )
where

import Data.Text (Text)
import Data.Traversable (for)
import Effectful
import Effectful.State.Static.Shared (State, evalState, get, put)
import GHC.Exception (CallStack, SrcLoc (..), getCallStack)
import GHC.Stack (HasCallStack, callStack)
import Loki.Prelude

data Producer a where
  Var :: Location -> a -> Producer a
  Const :: Location -> Const -> Producer a
  Do :: Location -> a -> Statement a -> Producer a

deriving stock instance (Show a) => Show (Producer a)

instance HasLocation (Producer a) where
  location (Var loc _) = loc
  location (Const loc _) = loc
  location (Do loc _ _) = loc

data Consumer a where
  Finish :: Location -> Consumer a
  Label :: Location -> a -> Consumer a

deriving stock instance (Show a) => Show (Consumer a)

instance HasLocation (Consumer a) where
  location (Finish loc) = loc
  location (Label loc _) = loc

data Statement a where
  Prim :: Location -> Text -> [Producer a] -> Consumer a -> Statement a
  Switch :: Location -> Producer a -> [(Const, Statement a)] -> Statement a -> Statement a
  Cut :: Location -> Producer a -> Consumer a -> Statement a

deriving stock instance (Show a) => Show (Statement a)

instance HasLocation (Statement a) where
  location (Prim loc _ _ _) = loc
  location (Switch loc _ _ _) = loc
  location (Cut loc _ _) = loc

prim :: (State Unique :> es) => Text -> [Producer Name] -> Eff es (Producer Name)
prim name args = do
  label <- newName name
  return $ Do locCS label (Prim locCS name args (Label locCS label))

switch :: (State Unique :> es) => Producer Name -> [(Const, Eff es (Producer Name))] -> Producer Name -> Eff es (Producer Name)
switch expr clauses def = do
  label <- newName "switch"
  clauses' <- for clauses \(c, p) -> do
    p' <- p
    return (c, Cut locCS p' (Label locCS label))
  return
    $ Do locCS label
    $ Switch locCS expr clauses'
    $ Cut locCS def (Label locCS label)

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
    $ prim "add" [Const locCS (Int 1), Const locCS (Int 2)]

ex2 :: Producer Name
ex2 =
  runPureEff
    $ evalState (Unique 0) do
      switch
        (Const locCS (Int 1))
        [(Int 1, prim "print" [Const locCS (Int 1)])]
        (Const locCS (Int 3))

data Location = forall a. (LocationRepr a) => Location a

instance Show Location where
  show (Location loc) = locationFile loc <> ":" <> show (locationLine loc) <> ":" <> show (locationColumn loc)

class HasLocation a where
  location :: a -> Location

class LocationRepr a where
  locationFile :: a -> String
  locationLine :: a -> Int
  locationColumn :: a -> Int

instance LocationRepr CallStack where
  locationFile cs =
    case getCallStack cs of
      [] -> "Unknown location"
      ((_, SrcLoc {srcLocFile}) : _) -> srcLocFile
  locationLine cs =
    case getCallStack cs of
      [] -> 0
      ((_, SrcLoc {srcLocStartLine}) : _) -> srcLocStartLine
  locationColumn cs =
    case getCallStack cs of
      [] -> 0
      ((_, SrcLoc {srcLocStartCol}) : _) -> srcLocStartCol

-- | locCS is a helper function that creates a 'Location' from the current call stack.
locCS :: (HasCallStack) => Location
locCS = Location callStack