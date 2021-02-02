{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Data.BDD.Visualization (toDot) where

import Data.BDD
import Data.Text.Lazy (Text)
import Data.Map ((!))
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

toDot :: BDD -> Text
toDot bdd@(BDD {treeMap, invMap, root}) = 
  toLazyText $ startText <> nodes <> edges <> endText
    where 
      startText = fromText "digraph G {\n"
      endText = fromText "}\n"
      nodes = foldr (<>) (fromText "") $ fst $ collectNodes (treeMap ! root) root Set.empty
      edges = foldr (<>) (fromText "") $ fst $ collectEdges (treeMap ! root) root Set.empty

      collectNodes (Branch n l r) nid seen
        | Set.member nid seen = ([], seen)
        | nid `elem` [0,1] = ([square nid], Set.insert nid seen)
        | otherwise = let (left, seen')  = collectNodes (treeMap ! l) l seen
                          (right, seen'') = collectNodes (treeMap ! r) r seen'
                       in ((circle nid n):(left++right), Set.insert nid seen'')

      collectEdges (Branch n l r) nid seen
        | Set.member nid seen = ([], seen)
        | nid `elem` [0,1] = ([], seen)
        | otherwise = let (left, seen') = collectEdges (treeMap ! l) l seen
                          (right, seen'') = collectEdges (treeMap ! r) r seen'
                       in ((dotted nid l):(arrow nid r):(left++right), Set.insert nid seen'')

      square nid = (fromString $ show nid) <> fromText " [shape=square]\n"
      circle nid label = (fromString $ show nid) <> fromText " [square=circle label=" <> fromString "X_" <> (fromString $ show label) <> fromText "]\n"

      arrow a b  = (fromString $ show a) <> fromText " -> " <> (fromString $ show b) <> fromText"\n"
      dotted a b = (fromString $ show a) <> fromText " -> " <> (fromString $ show b) <> fromText" [style=dotted]\n"

      

