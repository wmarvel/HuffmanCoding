module Lib
  ( HuffmanCode
  , fromList
  , encodeToList
  ) where

import Data.Char
import Data.Eq
import Data.List
import Data.Maybe
import Data.Ord

data HuffmanCode a b
  = HuffmanLeaf a
                b
  | HuffmanNode [a]
                b
                (HuffmanCode a b)
                (HuffmanCode a b)
  deriving (Eq, Show)

-- The Ord instance is explicitly specified because the derived Ord instance
-- does not compare weights first.
instance (Ord a, Real b) => Ord (HuffmanCode a b) where
  compare (HuffmanLeaf v1 w1) (HuffmanLeaf v2 w2) =
    if w1 /= w2
      then compare w1 w2
      else compare v1 v2
  compare (HuffmanNode _ w1 _ _) (HuffmanLeaf _ w2) =
    if w1 /= w2
      then compare w1 w2
      else GT
  compare (HuffmanLeaf _ w1) (HuffmanNode _ w2 _ _) =
    if w1 /= w2
      then compare w1 w2
      else LT
  compare (HuffmanNode s1 w1 l1 r1) (HuffmanNode s2 w2 l2 r2) =
    if w1 /= w2
      then compare w1 w2
      else if s1 /= s2
             then compare s1 s2
             else if l1 /= l2
                    then compare l1 l2
                    else compare r1 r2

-- | Return the weight of a HuffmanCode
weight :: (Ord a, Real b) => HuffmanCode a b -> b
weight (HuffmanLeaf _ w) = w
weight (HuffmanNode _ w _ _) = w

-- | Return the values of a HuffmanCode as a list
values :: HuffmanCode a b -> [a]
values (HuffmanLeaf v _) = v : []
values (HuffmanNode l _ _ _) = l

-- | Return true if the value given is one of the values in the HuffmanCode
contains :: (Eq a) => a -> HuffmanCode a b -> Bool
contains v (HuffmanLeaf lv _) = v == lv
contains v (HuffmanNode l _ _ _) = elem v l

-- | Make a leaf node from a list of identical values
makeLeaf :: [a] -> HuffmanCode a Int
makeLeaf s = HuffmanLeaf (head s) (length s)

-- | Make a new HuffmanCode by combining two HuffmanCodes. The two codes
-- must be disjoint.
makeNode ::
     (Ord a, Real b) => HuffmanCode a b -> HuffmanCode a b -> HuffmanCode a b
makeNode c1 c2 =
  HuffmanNode (values c1 ++ values c2) (weight c1 + weight c2) c1 c2

-- | Make a list of leafs from a list of values
makeLeaves :: (Ord a) => [a] -> [HuffmanCode a Int]
makeLeaves s = (sort (map makeLeaf (groupBy (==) (sort s))))

-- | Collapse a list of leaves into a single HuffmanCode
collapseCode :: (Ord a, Real b) => [HuffmanCode a b] -> HuffmanCode a b
collapseCode nodes =
  if length nodes == 1
    then head nodes
    else let n1 = head nodes
             n2 = head (drop 1 nodes)
         in collapseCode (sort ((makeNode n1 n2) : (drop 2 nodes)))

-- | Create a HuffmanCode from a list of values
fromList :: (Ord a) => [a] -> Maybe (HuffmanCode a Int)
fromList l =
  if length l < 1
    then Nothing
    else Just (collapseCode (makeLeaves l))

-- | Walk a HuffmanCode tree, creating a result by passing a provided
-- function False if we're taking the left branch and True if we're taking
-- the right branch
walkTree ::
     (Ord a, Real b) => HuffmanCode a b -> a -> (Bool -> c -> c) -> c -> Maybe c
walkTree hc sym fun out = go hc out
  where
    go (HuffmanLeaf lsym _) result
      | sym == lsym = Just result
    go (HuffmanNode _ _ left right) result
      | contains sym left = go left $ fun False result
      | contains sym right = go right $ fun True result
    go _ _ = Nothing

-- | Return the code representing a symbol as a list of booleans 
encodeToList :: (Ord a, Real b) => Maybe (HuffmanCode a b) -> a -> Maybe [Bool]
encodeToList mhc sym = fmap reverse $ mhc >>= walk
  where
    walk hc = walkTree hc sym (:) []
