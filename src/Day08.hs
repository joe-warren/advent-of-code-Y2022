{-# Language OverloadedStrings #-}
module Day08 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Data.Text as T
import Data.Char (isNumber, digitToInt)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.List (transpose, tails)
import Data.Functor ((<&>))
import Data.Foldable (foldl1)

parser :: Parser [[Int]]
parser =  (fmap digitToInt. T.unpack <$> MP.takeWhile1P Nothing isNumber) `MP.sepBy` "\n"

part1 :: [[Int]] -> Int
part1 t = 
    let allRotations = (id, id) :| [(fmap reverse, fmap reverse), (transpose, transpose), (transpose.reverse, reverse.transpose)]
        isVisible t = [all (< h) ts | (h:ts) <- tails t ] 
        visibleMaps = allRotations <&> (\(fwd, rev) -> (rev . fmap isVisible . fwd $ t))
    in length . filter id . concat . foldl1 (zipWith (zipWith (||))) $ visibleMaps

-- cheeky variant of take while which also returns the value that fails the predicate
-- I'm pretty sure this has a name, but I don't know what it is
takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
takeWhilePlus1 f (x:xs) | f x = x : takeWhilePlus1 f xs
                    | otherwise = [x]
takeWhilePlus1 _ [] = [] 

part2 :: [[Int]] -> Int
part2 t = 
    let allRotations = (id, id) :| [(fmap reverse, fmap reverse), (transpose, transpose), (transpose.reverse, reverse.transpose)]
        oneDirectionScore t = [length . takeWhilePlus1 (< h) $ ts | (h:ts) <- tails t ] 
        scoreMaps = allRotations <&> (\(fwd, rev) -> (rev . fmap oneDirectionScore . fwd $ t))
    in maximum . concat . foldl1 (zipWith (zipWith (*))) $ scoreMaps

main :: IO ()
main = withParser "data/day08.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)