{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Prop where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Set (Set)
import qualified Data.Set as Set

type Var = Int
type Env = Set Var

data Prop
  = PTrue
  | PFalse
  | PVar Var
  | PNeg Prop
  | PAnd Prop Prop
  | POr Prop Prop
  | PImpl Prop Prop
  | PEquiv Prop Prop
  deriving (Show, Eq)

makeBaseFunctor ''Prop

evalProp :: Env -> Prop -> Bool
evalProp env = cata $ \case
  PTrueF        -> True
  PFalseF       -> False
  PVarF s       -> Set.member s env
  PNegF p       -> not p
  PAndF p1 p2   -> p1 && p2
  POrF p1 p2    -> p1 || p2
  PImplF p1 p2  -> (not p1) || p2
  PEquivF p1 p2 -> p1 == p2

propFromBool :: Bool -> Prop
propFromBool True  = PTrue
propFromBool False = PFalse 

vars :: Prop -> Set Var
vars = cata $ \case
  PVarF s       -> Set.singleton s
  PTrueF        -> Set.empty
  PFalseF       -> Set.empty
  PNegF p       -> p
  PAndF p1 p2   -> p1 <> p2
  POrF p1 p2    -> p1 <> p2
  PImplF p1 p2  -> p1 <> p2
  PEquivF p1 p2 -> p1 <> p2

subst :: Var -> Bool -> Prop -> Prop
subst var val = cata $ \case
  PVarF s       -> if var == s then propFromBool val else PVar s
  PTrueF        -> PTrue
  PFalseF       -> PFalse
  PNegF p       -> PNeg p
  PAndF p1 p2   -> PAnd p1 p2
  POrF p1 p2    -> POr p1 p2
  PImplF p1 p2  -> PImpl p1 p2
  PEquivF p1 p2 -> PEquiv p1 p2

eval :: Prop -> Bool
eval = cata $ \case
  PTrueF        -> True
  PFalseF       -> False
  PVarF s       -> error $ "unbound variable "  ++ (show s)
  PNegF p       -> not p
  PAndF p1 p2   -> p1 && p2
  POrF p1 p2    -> p1 || p2
  PImplF p1 p2  -> (not p1) || p2
  PEquivF p1 p2 -> p1 == p2


sat :: Prop -> Set (Set Var)
sat p = Set.fromList 
  [x | x <- Set.toList. Set.powerSet . Set.fromList $ [1..(maximum $ vars p)], evalProp x p == True]
