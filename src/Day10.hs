{-# Language OverloadedStrings #-}
module Day10 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Applicative ((<|>))
import Data.Functor (($>), void)
import Data.Foldable (foldlM)
import Data.List.Split (chunksOf)
import Data.Bool (bool)
data Instruction = Noop | AddX Int deriving Show

instructionParser :: Parser Instruction
instructionParser = 
    let signedParser = MPL.signed MP.space MPL.decimal
    in (Noop <$ "noop") <|> (AddX <$> ("addx" *> MP.space *> signedParser))

parser :: Parser [Instruction]
parser = instructionParser `MP.sepBy` "\n"

run :: Instruction -> Int -> ([Int], Int)
run Noop x = ([x], x) 
run (AddX d) x = ([x, x], x + d) 

runCycles :: [Instruction] -> [Int]
runCycles = fst . foldlM (flip run) 1

relevantCycles :: [a] -> [a]
relevantCycles cs = 
    let go [] = []
        go (x:xs) = x:(go $ drop 39 xs)
    in go (drop 19 cs) 

part1 :: [Instruction] -> Int
part1 =  sum . fmap (uncurry (*)) .  relevantCycles . zip [1..] .runCycles

isOn :: (Int, Int) -> Bool
isOn (cycleId, x) = abs ((cycleId `mod` 40) - x) <= 1

part2 :: [Instruction]-> [String]
part2 =  chunksOf 40 . fmap (bool ' ' '■' . isOn) . zip [0..] .runCycles

main :: IO ()
main = withParser "data/day10.txt" parser $ \res -> do
            print (part1 res)
            void $ traverse putStrLn (part2 res) 

