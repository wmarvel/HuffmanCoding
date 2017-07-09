module Main where

import HuffmanCode

-- Spit out the huffman coding of 'Hello, World'
main :: IO ()
main = do
  mapM_ putCoding $ huffmanCoded hello
  putStrLn ""

hello :: String
hello = "Hello, World"

asBitChar :: Bool -> Char
asBitChar True = '1'
asBitChar False = '0'

asBitStr :: [Bool] -> String
asBitStr = map asBitChar

putCoding :: Maybe String -> IO ()
putCoding = maybe (putStr "") putStr

huffmanCoded :: String -> [Maybe String]
huffmanCoded s = fmap asBitStr <$> map (listCoder s) s

