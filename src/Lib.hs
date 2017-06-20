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

data HuffmanCode a
  = HuffmanLeaf Int
                a
  | HuffmanNode Int
                [a]
                (HuffmanCode a)
                (HuffmanCode a)
  deriving (Eq, Show)

-- The Ord instance is explicitly specified because the derived Ord instance
-- does not compare weights first.
instance (Ord a) => Ord (HuffmanCode a) where
  compare (HuffmanLeaf w1 v1) (HuffmanLeaf w2 v2) =
    if w1 /= w2
      then compare w1 w2
      else compare v1 v2
  compare (HuffmanNode w1 _ _ _) (HuffmanLeaf w2 _) =
    if w1 /= w2
      then compare w1 w2
      else GT
  compare (HuffmanLeaf w1 _) (HuffmanNode w2 _ _ _) =
    if w1 /= w2
      then compare w1 w2
      else LT
  compare (HuffmanNode w1 s1 l1 r1) (HuffmanNode w2 s2 l2 r2) =
    if w1 /= w2
      then compare w1 w2
      else if s1 /= s2
             then compare s1 s2
             else if l1 /= l2
                    then compare l1 l2
                    else compare r1 r2

weight :: HuffmanCode a -> Int
weight (HuffmanLeaf w _) = w
weight (HuffmanNode w _ _ _) = w

values :: HuffmanCode a -> [a]
values (HuffmanLeaf _ v) = v : []
values (HuffmanNode _ l _ _) = l

contains :: (Eq a) => a -> HuffmanCode a -> Bool
contains v (HuffmanLeaf _ lv) = v == lv
contains v (HuffmanNode _ l _ _) = elem v l

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

walkTree ::
     (Ord a) => HuffmanCode a -> a -> b -> (b -> b) -> (b -> b) -> Maybe b
walkTree hc sym out lfun rfun = go hc out
  where
    go (HuffmanLeaf _ lsym) result =
      if sym == lsym
        then Just result
        else Nothing
    go (HuffmanNode _ _ left right) result =
      if (contains sym left)
        then go left (lfun result)
        else if (contains sym right)
               then go right (rfun result)
               else Nothing

encodeToList :: (Ord a) => Maybe (HuffmanCode a) -> a -> Maybe [Bool]
encodeToList Nothing sym = Nothing
encodeToList mhc sym =
  case (walkTree (fromJust mhc) sym [] (\x -> False : x) (\x -> True : x)) of
    Nothing -> Nothing
    Just list -> Just (reverse list)
