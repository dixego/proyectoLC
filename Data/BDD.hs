--{-# LANGUAGE TemplateHaskell   #-}
--{-# LANGUAGE LambdaCase        #-}
--{-# LANGUAGE TypeFamilies      #-}
--{-# LANGUAGE DeriveFunctor     #-}
--{-# LANGUAGE DeriveFoldable    #-}
--{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns    #-}

import Data.Prop
import Data.Map (Map)
import qualified Data.Map as Map

type NodeRef = Int

data BDDNode
  = Zero                       -- false
  | One                        -- true
  | Branch Var NodeRef NodeRef -- Branch x_i lo hi
  deriving (Show, Eq)

data BDD = BDD {
  treeMap :: Map NodeRef BDDNode, -- map from node references to actual nodes (tree)
  invMap  :: Map BDDNode NodeRef, -- map from nodes to node references
  root    :: NodeRef              -- root of the tree
               }

-- assuming:
-- * prop is a propositional formula with n variables numbered 1,2,...n
-- produces:
-- * a reduced ordered BDD representing the same boolean function as prop
--   where the order is determined by prop's variable numbering
expr2bdd :: Prop -> Int -> BDD
expr2bdd prop n = expr2bddAux prop n initBDD 1
  where initBDD = BDD (Map.fromList [(0, Zero), (1, One)]) Map.empty 0

expr2bddAux :: Prop -> Int -> BDD -> Var -> BDD
expr2bddAux prop n bdd@(BDD { treeMap, invMap, root}) i
  | i > n = undefined
  | otherwise = undefined
