{-# LANGUAGE RecordWildCards #-}

module Construction.Internal.Types
  ( Name, Term(..)
  , Type (..), Context (..), Substitution (..)
  , Equation
  ) where

import Data.Text (Text, unpack) -- we want to import only Text from Data.Text.
import Data.Map  (Map (..), toAscList)
import Data.Set  (Set (..))


type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M


data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Eq, Ord)

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables
  deriving (Eq)

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type
  deriving (Eq)

type Equation = (Type, Type)
--data Equation = Equatation {l :: Type, r :: Type}
  --deriving (Eq, Ord)

instance Monoid Context where
  mempty = Context mempty
  Context a `mappend` Context b = Context $ a `mappend` b

instance Monoid Substitution where
  mempty = Substitution mempty
  Substitution a `mappend` Substitution b = Substitution $ a `mappend` b

instance Show Term where
  --show = bshow False

  --bshow :: Term -> (String


  show (Var n) = unpack n
  show (App a@Var{..} b@(Var _)) = show a ++ " " ++ show b
  show (App a@(App _ Lam{..}) b@Var{..}) = "(" ++ show a ++ ") " ++ show b
  show (App a@App{..} b@Var{..}) = show a ++ " " ++ show b
  show (App a@Lam{..} b@Var{..}) = "(" ++ show a ++ ") " ++ show b
  show (App a@Var{..} b@App{..}) = show a ++ " (" ++ show b ++ ")"
  show (App a@(App _ Lam{..}) b@(App _ _)) = "(" ++ show a ++ ") (" ++ show b ++ ")"
  show (App a@App{..} b@(App _ _)) = show a ++ " (" ++ show b ++ ")"
  show (App a@Lam{..} b@App{..}) = "(" ++ show a ++ ") (" ++ show b ++ ")"
  show (App a@Var{..} b@Lam{..}) = show a ++ " " ++ show b
  show (App a@(App _ Lam{..}) b@(Lam _ _)) = "(" ++ show a ++ ") " ++ show b
  show (App a@App{..} b@Lam{..}) = show a ++ " " ++ show b 
  show (App a@Lam{..} b@(Lam _ _)) = "(" ++ show a ++ ") " ++ show b
  show (Lam n t) = "\\" ++ unpack n ++ "." ++ show t

instance Show Type where
  show (TVar n) = unpack n
  show (TArr from@(TArr _ _) to) = "(" ++ show from ++ ") -> " ++ show to
  show (TArr from to) = show from ++ " -> " ++ show to

instance Show Context where
  show (Context m) = pl $ toAscList m where
    pl :: [(Name, Type)] -> String
    one n t = unpack n ++ " : " ++ show t
    pl [(n, t)] = one n t
    pl ((n, t):xs) = one n t ++ ", " ++ pl xs

instance Show Substitution where
  show (Substitution m) = pl $ toAscList m where
    pl :: [(Name, Type)] -> String
    one n t = unpack n ++ " = " ++ show t
    pl [(n, t)] = one n t
    pl ((n, t):xs) = one n t ++ ", " ++ pl xs

--instance Show Equation where
--  show (Equatation t1 t2) = show t1 ++ " = " ++ show t2
