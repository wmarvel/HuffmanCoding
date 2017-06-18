module Lib
    ( HuffmanCode
    , fromList
    ) where

import Data.Eq
import Data.Ord
import Data.Char
import Data.List
import Data.Maybe

data HuffmanCode a = HuffmanLeaf Int a
  | HuffmanNode Int [a] (HuffmanCode a) (HuffmanCode a)
  deriving (Eq, Ord, Show)

weight :: HuffmanCode a -> Int
weight (HuffmanLeaf w _) = w
weight (HuffmanNode w _ _ _) = w

values :: HuffmanCode a -> [a]
values (HuffmanLeaf _ v) = v:[]
values (HuffmanNode _ l _ _) = l

contains :: (Eq a) => a -> HuffmanCode a -> Bool
contains v (HuffmanLeaf _ lv) = v == lv
contains v (HuffmanNode _ l _ _) = any (==v) l

makeLeaf :: [a] -> HuffmanCode a
makeLeaf s = HuffmanLeaf (length s) (head s)

makeNode :: HuffmanCode a -> HuffmanCode a -> HuffmanCode a
makeNode c1 c2 =
  HuffmanNode (weight c1 + weight c2) (values c1 ++ values c2) c1 c2

makeLeaves :: (Ord a) => [a] -> [HuffmanCode a]
makeLeaves s = sort (map makeLeaf (groupBy (==) (sort s)))

collapseCode :: (Ord a) => [HuffmanCode a] -> HuffmanCode a
collapseCode nodes =
   if length nodes == 1
   then head nodes
   else let n1 = head nodes
            n2 = head (drop 1 nodes)
   in collapseCode (sort ((makeNode n1 n2) : (drop 2 nodes)))

fromList :: (Ord a) => [a] -> Maybe (HuffmanCode a)
fromList l =
   if (length l) < 1
   then Nothing
   else Just (collapseCode (makeLeaves l))
