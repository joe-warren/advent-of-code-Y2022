{-# Language OverloadedStrings #-}
module Day03 (main) where

import CommonParsingStuff
import Control.Applicative (some)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Ord (Down (..))
import Data.List (sortOn, intersect, group, foldl1')
import Data.Char (isLower, ord)
import qualified Data.Set as S
import Data.List.Split (chunksOf)

parser :: Parser [([Char], [Char])]
parser = (postProcess <$> some MP.letterChar) `MP.sepBy` "\n"
  where
    postProcess xs = let l = length xs `div` 2
                      in (take l xs, drop l xs)

score :: Char -> Int
score x | isLower x = 1 + ord x - ord 'a'
        | otherwise = 27 + ord x - ord 'A'

commonLetters :: [Char] -> [Char] -> [Char]
commonLetters a b = S.toList $ S.intersection (S.fromList a) (S.fromList b)

part1 :: [([Char], [Char])] -> Int
part1 = sum . fmap ( sum . fmap score . uncurry commonLetters)

unionN :: [[Char]] -> [Char]
unionN = S.toList . foldl1' S.intersection . fmap S.fromList

-- This is stupid, because I went to all the effort of doing the splitting in the parser in part 1
-- and then "reassemble and resplit" here
-- I really hate the AoC format sometimes
part2 :: [([Char], [Char])] -> Int
part2 = sum . fmap (sum . fmap score . unionN) . chunksOf 3 . fmap (uncurry (++))

main :: IO ()
main = withParser "data/day03.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)
