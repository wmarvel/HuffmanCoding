
import Lib
import Data.Maybe
import Data.Tuple
import Test.Hspec

-- From http://mathworld.wolfram.com/TotallyOrderedSet.html ->
-- A relation <= is a total order on a set S ("<= totally orders  S")
-- if the following properties hold: 
-- 1. Reflexivity: a<=a for all a in S.
-- 2. Antisymmetry: a<=b and b<=a implies  a=b.
-- 3. Transitivity: a<=b and b<=c implies  a<=c.
-- 4. Comparability (trichotomy law): For any a,b in S, either a<=b or b<=a.
-- Can't test for all possible of all types.
-- Would a test in some specific defined subset suffice? Not really, but
-- at least we have some shot of finding a break rather than not testing
-- at all

-- This seems a little bit silly
reflexivity :: (Ord a) => Maybe (HuffmanCode a) -> Bool
reflexivity mhc = case mhc of
  Just hc -> hc <= hc && hc == hc
  _ -> False

-- Simple test based on the following table taken from
-- https://en.wikipedia.org/wiki/Huffman_coding
-- Symbol    (ai) a    b    c    d    e
-- Weights   (wi) 0.10 0.15 0.30 0.16 0.29
-- Codewords (ci) 010  011  11   00   10
simpleTest :: Bool
simpleTest =
  let testcode =
        fromList
          (replicate 10 'a' ++
           replicate 15 'b' ++
           replicate 30 'c' ++ replicate 16 'd' ++ replicate 29 'e')
      testchars = "abcde"
      expected =
        [ Just [False, True, False]
        , Just [False, True, True]
        , Just [True, True]
        , Just [False, False]
        , Just [True, False]
        ]
  in map (encodeToList testcode) testchars == expected

main :: IO ()
main =
  hspec $ do
    describe "HuffmanCode" $ do
      it "generate the correct codings for a simple test" $
        simpleTest `shouldBe` True
    describe "HuffmanCoding Ord" $ do
      it "has at least one Leaf with the Reflexivity property" $
        reflexivity (fromList "a") `shouldBe` True
      it "has at least one Node with the Reflexivity property" $
        reflexivity (fromList "ab") `shouldBe` True
