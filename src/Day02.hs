
{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}
module Day02 (main) where

import CommonParsingStuff
import Data.Functor (($>))
import Control.Applicative (some, (<|>))
import Control.Arrow ((&&&))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Ord (Down (..))
import Data.List (sortOn)

data Throw = Rock | Paper | Scissors deriving (Eq, Show) 

-- next a beats a
next :: Throw -> Throw
next Rock = Paper
next Paper = Scissors 
next Scissors = Rock

-- prev b loses to b
prev :: Throw -> Throw
prev Rock = Scissors
prev Paper = Rock
prev Scissors = Paper

throwParser :: Parser a -> Parser b -> Parser c -> Parser Throw
throwParser r p s = (r $> Rock) <|> (p $> Paper) <|> (s $> Scissors)

part1Parser :: Parser [(Throw, Throw)]
part1Parser = 
    let 
        theirParser = throwParser "A" "B" "C"
        ourParser = throwParser "X" "Y" "Z"
        line = ((,) <$>  (theirParser <* " ") <*> ourParser) 
     in line `MP.sepBy` "\n"

scoreFromThrow :: Throw -> Int
scoreFromThrow Rock = 1
scoreFromThrow Paper = 2
scoreFromThrow Scissors = 3

scoreFromPlay :: Throw -> Throw -> Int
scoreFromPlay theirs ours | theirs == ours = 3
                          | theirs == prev ours = 6 
                          | otherwise = 0

scoreRound :: (Throw, Throw) -> Int
scoreRound = (+) <$> (scoreFromThrow . snd) <*> (uncurry scoreFromPlay)

-- part 2

data Outcome = Lose | Draw | Win deriving (Eq, Show) 

outcomeParser :: Parser Outcome
outcomeParser = ("X" $> Lose) <|> ("Y" $> Draw) <|> ("Z" $> Win)

part2Parser :: Parser [(Throw, Outcome)]
part2Parser = 
    let theirParser = throwParser "A" "B" "C"
        line = ((,) <$> ( theirParser <* " ") <*> outcomeParser) 
     in line `MP.sepBy` "\n"

throwForOutcome :: Throw -> Outcome -> Throw
throwForOutcome t Draw = t
throwForOutcome t Lose = prev t
throwForOutcome t Win = next t

part2Score :: (Throw, Outcome) -> Int
part2Score = scoreRound . (fst &&& uncurry throwForOutcome)
    --let ours = throwForOutcome theirs outcome
     --- in scoreRound (theirs, ours)                          

main :: IO ()
main = do
    withParser "data/day02.txt" part1Parser $ \res -> do
       print (sum $ scoreRound <$> res)
    withParser "data/day02.txt" part2Parser $ \res -> do
       print (sum $ part2Score <$> res)