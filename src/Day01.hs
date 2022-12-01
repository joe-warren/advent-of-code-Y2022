{-# Language OverloadedStrings #-}
module Day01 (main) where

import CommonParsingStuff
import Control.Applicative (some)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Ord (Down (..))
import Data.List (sortOn)

parser :: Parser [[Int]]
parser = some (MP.decimal <* "\n") `MP.sepBy` "\n"

part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortOn Down . fmap sum

main :: IO ()
main = withParser "data/day01.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)
