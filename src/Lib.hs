module Lib
    ( HuffmanCode
    , fromString
    ) where

import Data.Eq
import Data.Ord
import Data.Char
import Data.List
import Data.Maybe

data HuffmanCode = HuffmanLeaf Int Char
  | HuffmanNode Int String HuffmanCode HuffmanCode
  deriving (Eq, Ord, Show)

-- Deriving automatically builds stuff somewhat like 
-- instance Ord HuffmanCode where
--   (HuffmanLeaf _ w1) `compare` (HuffmanLeaf _ w2) = w1 `compare` w2
--   (HuffmanLeaf _ w1) `compare` (HuffmanNode _ w2 _ _) = w1 `compare` w2
--   (HuffmanNode _ w1 _ _) `compare` (HuffmanLeaf _ w2) = w1 `compare` w2
--   (HuffmanNode _ w1 _ _) `compare` (HuffmanNode _ w2 _ _) = w1 `compare` w2
-- except it builds all the cases for all of the things
-- We put the Int at the start of our types because it seems to compare in
-- the order of the values of those types

weight :: HuffmanCode -> Int
weight (HuffmanLeaf w _) = w
weight (HuffmanNode w _ _ _) = w

chars :: HuffmanCode -> String
chars (HuffmanLeaf _ c) = c:[]
chars (HuffmanNode _ s _ _) = s

contains :: Char -> HuffmanCode -> Bool
contains c (HuffmanLeaf _ lc) = c == lc
contains c (HuffmanNode _ s _ _) = any (==c) s

makeLeaf :: [Char] -> HuffmanCode
makeLeaf s = HuffmanLeaf (length s) (head s)

makeNode :: HuffmanCode -> HuffmanCode -> HuffmanCode
makeNode c1 c2 =
  HuffmanNode (weight c1 + weight c2) (chars c1 ++ chars c2) c1 c2

makeLeaves :: String -> [HuffmanCode]
makeLeaves s = sort (map makeLeaf (groupBy (==) (sort s)))

collapseCode :: [HuffmanCode] -> HuffmanCode
collapseCode nodes =
  if length nodes == 1
  then head nodes
  else let n1 = head nodes
           n2 = head (drop 1 nodes)
  in collapseCode (sort ((makeNode n1 n2) : (drop 2 nodes)))

fromString :: String -> Maybe HuffmanCode
fromString s =
  if (length s) < 1
  then Nothing
  else Just (collapseCode (makeLeaves s))

