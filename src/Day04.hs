{-# Language OverloadedStrings #-}
module Day04 (main) where

import CommonParsingStuff
import Control.Applicative (some)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Ord (Down (..))
import Data.List (sortOn)

parser :: Parser [((Int, Int), (Int, Int))]
parser = let range = (,) <$> (MP.decimal <* "-") <*> MP.decimal
             pair = (,) <$> (range <* ",") <*> range
          in  pair `MP.sepBy` "\n"

contained :: ((Int, Int), (Int, Int)) -> Bool
contained ((lo1, hi1), (lo2, hi2)) = (lo1 <= lo2 && hi1 >= hi2) || (lo2 <= lo1 && hi2 >= hi1)

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 = length . filter contained

overlaps ::  ((Int, Int), (Int, Int)) -> Bool
overlaps ((lo1, hi1), (lo2, hi2)) = (lo1 <= lo2 && hi1 >= lo2) || (lo1 >= lo2 && lo1 <= hi2)

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 = length . filter overlaps

main :: IO ()
main = withParser "data/day04.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)
