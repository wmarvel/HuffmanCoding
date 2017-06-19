module Lib
    ( HuffmanCode
    , fromList
    , encodeToList
    ) where

import Data.Eq
import Data.Ord
import Data.Char
import Data.List
import Data.Maybe

data HuffmanCode a = HuffmanLeaf Int a
  | HuffmanNode Int [a] (HuffmanCode a) (HuffmanCode a)
  deriving (Eq, Show)

-- There's a table in the Wikipedia page on Huffman coding that gives:
-- Symbol    (ai)    a    b    c    d    e
-- Weights   (wi)    0.10 0.15 0.30 0.16 0.29
-- Codewords (ci)    010  011  11   00   10
-- If an ordering that is not total, using only the weight, is used,
-- we get the above as output.
-- If I use the total ordering of the derived Ord, the table for this
-- implementation looks like
-- Symbol    (ai)    a    b    c    d    e
-- Weights   (wi)    0.10 0.15 0.30 0.16 0.29
-- Codewords (ci)    110  111  10   00   01
-- Which means we are not ordering by weight at all times as these weights
-- are all different. Therefore, we cannot use the derived Ord
-- Ord needs to impose a total ordering
-- 1. Reflexivity: a<=a for all a in S.
-- 2. Antisymmetry: a<=b and b<=a implies  a=b.
-- 3. Transitivity: a<=b and b<=c implies  a<=c.
-- 4. Comparability (trichotomy law): For any a,b in S, either a<=b or b<=a.
-- This Ord instance is defined to compare on weight *first* with intent to
-- meet the total ordering requirement
-- NEED TO WRITE TESTS FOR THE LAWS
instance (Ord a) => Ord (HuffmanCode a) where
   compare (HuffmanLeaf w1 c1) (HuffmanLeaf w2 c2) =
     if w1 /= w2 then compare w1 w2 else compare c1 c2
   compare (HuffmanNode w1 _ _ _) (HuffmanLeaf w2 _) =
     if w1 /= w2 then compare w1 w2 else GT
   compare (HuffmanLeaf w1 _) (HuffmanNode w2 _ _ _) =
     if w1 /= w2 then compare w1 w2 else LT
   compare (HuffmanNode w1 s1 l1 r1) (HuffmanNode w2 s2 l2 r2) =
     if w1 /= w2 then compare w1 w2
     else if s1 /= s2 then compare s1 s2
     else if l1 /= l2 then compare l1 l2
     else if r1 /= r2 then compare r1 r2
     else EQ

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
makeLeaves s = (sort (map makeLeaf (groupBy (==) (sort s))))

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

encodeToList :: (Ord a) => Maybe (HuffmanCode a) -> a -> Maybe [Bool]
encodeToList Nothing v = Nothing
encodeToList mhc v = go (fromJust mhc) v []
  where
    go (HuffmanLeaf _ lv) v l =
      if v == lv then Just (reverse l) else Nothing
    go (HuffmanNode _ _ left right) v l =
      if (contains v left) then go left v (False : l)
      else if (contains v right) then go right v (True : l)
      else Nothing
