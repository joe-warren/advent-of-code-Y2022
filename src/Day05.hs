{-# Language OverloadedStrings #-}
module Day05 (main) where

import CommonParsingStuff
import Control.Applicative (some, (<|>))
import Data.Functor (($>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Ord (Down (..))
import Data.List (sortOn, foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
newtype Stack = Stack {getStackMap :: M.Map Char [Char]} deriving (Show)

data Move = Move Int Char Char deriving Show

parser :: Parser (Stack, [Move])
parser = do
    let itemParser :: Parser ([Char] -> [Char])
        itemParser =  ((:) <$> ("[" *> MP.letterChar <* "]")) <|> (id <$ "   ")
    lines <- some ((itemParser `MP.sepBy` " ") <* "\n")
    names <- (" " *> MP.alphaNumChar <* " ") `MP.sepBy` " " 
    let cols = foldr (zipWith ($)) (names $> []) lines
    let s = Stack . M.fromList $ zip names cols
    _ <- "\n\n"
    let moveLine = Move <$> ("move " *> MP.decimal <* " from ") <*> (MP.alphaNumChar <* " to ") <*> MP.alphaNumChar
    moves <- moveLine `MP.sepBy` "\n"
    return (s, moves)

doMove :: ([Char] -> [Char]) -> Move -> Stack -> Stack
doMove transform (Move n src dest) (Stack m)= 
    let movedBlocks = fromMaybe [] $ transform . take n <$> M.lookup src m 
     in Stack . M.adjust (drop n) src . M.adjust (movedBlocks ++) dest $ m 

run :: ([Char] -> [Char]) -> (Stack, [Move]) -> Stack
run t (s, m) = foldl' (flip (doMove t)) s m 

part1 :: (Stack, [Move]) -> String
part1 = fmap head . toList . getStackMap . run reverse

part2 :: (Stack, [Move]) -> String
part2 = fmap head . toList . getStackMap . run id

main :: IO ()
main = withParser "data/day05.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)
