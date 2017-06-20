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

weight :: (Ord a, Real b) => HuffmanCode a b -> b
weight (HuffmanLeaf _ w) = w
weight (HuffmanNode _ w _ _) = w

values :: HuffmanCode a b -> [a]
values (HuffmanLeaf v _) = v : []
values (HuffmanNode l _ _ _) = l

contains :: (Eq a) => a -> HuffmanCode a b -> Bool
contains v (HuffmanLeaf lv _) = v == lv
contains v (HuffmanNode l _ _ _) = elem v l

makeLeaf :: [a] -> HuffmanCode a Int
makeLeaf s = HuffmanLeaf (head s) (length s)

makeNode ::
     (Ord a, Real b) => HuffmanCode a b -> HuffmanCode a b -> HuffmanCode a b
makeNode c1 c2 =
  HuffmanNode (values c1 ++ values c2) (weight c1 + weight c2) c1 c2

makeLeaves :: (Ord a) => [a] -> [HuffmanCode a Int]
makeLeaves s = (sort (map makeLeaf (groupBy (==) (sort s))))

collapseCode :: (Ord a, Real b) => [HuffmanCode a b] -> HuffmanCode a b
collapseCode nodes =
   if length nodes == 1
     then head nodes
     else let n1 = head nodes
              n2 = head (drop 1 nodes)
          in collapseCode (sort ((makeNode n1 n2) : (drop 2 nodes)))

fromList :: (Ord a) => [a] -> Maybe (HuffmanCode a Int)
fromList l =
   if (length l) < 1
     then Nothing
     else Just (collapseCode (makeLeaves l))

walkTree ::
     (Ord a, Real b)
  => HuffmanCode a b
  -> a
  -> c
  -> (c -> c)
  -> (c -> c)
  -> Maybe c
walkTree hc sym out lfun rfun = go hc out
  where
    go (HuffmanLeaf lsym _) result =
      if sym == lsym
        then Just result
        else Nothing
    go (HuffmanNode _ _ left right) result =
      if (contains sym left)
        then go left (lfun result)
        else if (contains sym right)
               then go right (rfun result)
               else Nothing

encodeToList :: (Ord a, Real b) => Maybe (HuffmanCode a b) -> a -> Maybe [Bool]
encodeToList Nothing sym = Nothing
encodeToList mhc sym =
  case (walkTree (fromJust mhc) sym [] (\x -> False : x) (\x -> True : x)) of
    Nothing -> Nothing
    Just list -> Just (reverse list)
