{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}
module Day06 (main) where

import qualified Data.Set as S

detectUniqueRun :: Int -> String -> Int
detectUniqueRun n input = go n input
  where
    isMessageHeader = (==n) . length . S.fromList . take n
    go i xss@(_:xs) | isMessageHeader xss = i
                   | otherwise = go (i+1) xs
    go _ _ = error "no message header"          
main :: IO ()
main = do
    contents <- readFile "data/day06.txt"
    print (detectUniqueRun 4 contents)
    print (detectUniqueRun 14 contents)