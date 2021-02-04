{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE BangPatterns      #-}

module Data.BDD (BDD(..), BDDNode(..), NodeRef, BoolOp, build, expr2bdd, apply, disj, conj, neg, sat, testSat) where

import Data.Prop hiding (sat)
import qualified Data.Prop  as P (sat)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bool (bool) 
import Data.List (subsequences)

import Debug.Trace

debug x = trace (show x) x

type BoolOp = Bool -> Bool -> Bool

type NodeRef = Int

data BDDNode = Branch Var NodeRef NodeRef -- Branch x_i lo hi
  deriving (Show, Eq, Ord)

data BDD = BDD {
  treeMap :: Map NodeRef BDDNode, -- map from node references to actual nodes (tree)
  invMap  :: Map BDDNode NodeRef, -- map from nodes to node references
  root    :: NodeRef              -- root of the tree
               } deriving (Show)

-- assuming:
-- * prop is a propositional formula with n variables numbered 1,2,...n
-- produces:
-- * a reduced ordered BDD representing the same boolean function as prop
--   where the order is determined by prop's variable numbering
expr2bdd :: Prop -> Int -> BDD
expr2bdd prop n = expr2bddAux prop n initBDD 1
  where 
    initBDD = BDD (Map.fromList [(0, Branch (n+1) 0 0), (1, Branch (n+1) 1 1)]) Map.empty 0

-- proxy for expr2bdd that automatically determines the number of 
-- variables in the formula by the highest number of its variables
build :: Prop -> BDD
build prop = expr2bdd prop n
  where 
    n = highestVar prop
    highestVar prop = 
      let v = vars prop
       in if null v then 0 else maximum . Set.toList $ v


-- build sub-BDD rooted at variable x_i
-- if i > n then all substitutions have been performed and prop can be evaluated to Zero
-- or One, otherwise recurse over lo and hi depending on substitution of x_i to False or
-- True respectively. BDD is implicitly reduced.
expr2bddAux :: Prop -> Int -> BDD -> Var -> BDD
expr2bddAux prop n bdd@(BDD { treeMap, invMap, root}) i
  | i > n = let root' = bool 0 1 (eval prop) in BDD treeMap invMap root'
  | otherwise = 
    let bdd1@(BDD _ _ r1) = expr2bddAux (subst i False prop) n bdd (i+1)
        bdd2@(BDD _ _ r2) = expr2bddAux (subst i True prop) n bdd1 (i+1)
    in subtree bdd2 i r1 r2


-- given a BDD, a variable number and a lo and hi references
-- returns the sub-BDD rooted at that node, performing a reduction if necesarry
-- i.e. if lo == hi then both outgoing edges from the node would go to the same
-- subtree, so we can just return that subtree; if a node (Branch i lo hi) already exists
-- then return the subtree rooted at that node; if it doesn't, update the tree creating
-- the node and return the tree rooted at the new node.
subtree :: BDD -> Var -> NodeRef -> NodeRef -> BDD
subtree bdd@(BDD {treeMap, invMap, root}) i lo hi 
  | lo == hi = bdd { root = lo }
  | Map.member (Branch i lo hi) invMap = bdd { root = invMap ! (Branch i lo hi) }
  | otherwise = 
    let !root'    = Map.size treeMap
        !treeMap' = Map.insert root' (Branch i lo hi) treeMap
        !invMap'  = Map.insert (Branch i lo hi) root' invMap
    in BDD treeMap' invMap' root'


-- applies a binary boolean operation to two BDDs, assuming they operate on the same
-- variables (i.e. with the same ordering).
apply :: BDD -> BDD -> BoolOp -> BDD
apply bdd1@(BDD _ _ r1) bdd2@(BDD _ _ r2) op = bdd
  where
    n1 = countVars bdd1
    n2 = countVars bdd2 
    n  = max n1 n2
    countVars (BDD bdd _ _ ) = 
      maximum $ 0:[i | (n, (Branch i _ _)) <- Map.toList bdd, not (elem n [1,0])]
    initBDD = BDD (Map.fromList [(0, Branch (n+1) 0 0), (1, Branch (n+1) 1 1)]) Map.empty 0
    (bdd, _) = app bdd1 bdd2 op initBDD Map.empty

type RefMap = Map (NodeRef, NodeRef) NodeRef

-- app uses a partially computed result plus a map for memoization
app :: BDD -> BDD -> BoolOp -> BDD -> RefMap -> (BDD, RefMap)
app bdd1@(BDD tm1 im1 r1) bdd2@(BDD tm2 im2 r2) op result g =
  let (result', g')
        | Map.member (r1, r2) g = (result { root = g ! (r1, r2)}, g)
        | r1 < 2 && r2 < 2      = (result { root = evalOp r1 r2 op}, g)
        | r1 < 2                = makeApply v2 r1 lo2 r1 hi2
        | r2 < 2                = makeApply v1 lo1 r2 hi1 r2
        | v1 == v2              = makeApply v1 lo1 lo2 hi1 hi2
        | v1 < v2               = makeApply v1 lo1 r2 hi1 r2
        | v2 < v1               = makeApply v2 r1 lo2 r1 hi2
   in (result', Map.insert (r1, r2) (root result') g')
  where 
    Branch v1 lo1 hi1 = tm1 ! r1
    Branch v2 lo2 hi2 = tm2 ! r2

    makeApply v l1 l2 h1 h2 =
      let !(r1, g1) = app (bdd1{root=l1}) (bdd2{root=l2}) op result g
          !(r2, g2) = app (bdd1{root=h1}) (bdd2{root=h2}) op r1 g1
          !r3 = subtree r2 v (root r1) (root r2)
      in (r3, g2)

    evalOp a b op
      | op a' b' = 1
      | otherwise = 0
        where a' = (a == 1)
              b' = (b == 1)

-- disjunction of two BDDs
disj :: BDD -> BDD -> BDD
disj b1 b2 = apply b1 b2 (||)

-- conjuction of two BDDs
conj :: BDD -> BDD -> BDD
conj b1 b2 = apply b1 b2 (&&)

-- negation of a BDD
neg :: BDD -> BDD
neg bdd@(BDD tMap iMap root) =
  bdd { treeMap = Map.map inv tMap,
        invMap  = Map.mapKeys inv iMap,
        root    = i root}
  where 
    inv b@(Branch x lo hi) = 
      if not (elem x [1,0]) then Branch x (i lo) (i hi) else b
    i 0 = 1
    i 1 = 0
    i n = n

sat :: BDD -> Set (Set Var) 
sat bdd@(BDD {treeMap, invMap, root}) = 
  Set.fromList (map Set.fromList $ sat' root (treeMap ! root) 1)
  where
    Branch n _ _ = treeMap ! 0
    nNodes = n-1

    sat' 0 (Branch _ _ _) _ = []
    sat' 1 (Branch n _ _) i
      | n > i = subsequences [i..nNodes]
      | otherwise = [[]]
    sat' _ (Branch x l h) i 
      | x > i =
        [y ++ z | y <- subsequences [i..(x-1)], z <- sat' l (treeMap ! l) (x+1)] ++
        [y ++ (x:z) | y <- subsequences [i..(x-1)], z <- sat' h (treeMap ! h) (x+1)]
      | otherwise  = sat' l (treeMap ! l) (x+1) ++ [x:r | r <- sat' h (treeMap ! h) (x+1)]

testSat :: Prop -> Bool
testSat p = P.sat p == (sat $ build p)
