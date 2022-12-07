{-# Language OverloadedStrings #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
module Day07 (main) where

import CommonParsingStuff
import Control.Monad (guard)
import Control.Applicative (some, many, (<|>))
import Data.Functor (($>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Data.Ord (Down (..))
import Data.List (sortOn, foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Monoid (Sum (..))
import Data.Semigroup (Min (..))

data Dir a = Dir { 
    dirName :: Text,
    dirSubdirs :: [Dir a],
    dirFiles :: [a]
    } deriving (Show, Foldable, Functor, Traversable)

parser :: Parser (Dir (Integer, Text)) 
parser = do
    let toEol = MP.takeWhile1P Nothing (/= '\n') <* "\n"
    name <- "$ cd" *> MP.space *> toEol
    guard (name /= "..")
    _ <- "$ ls\n"
    let file =  (,) <$> MPL.decimal <* MP.space <*> toEol
    let dir = ("dir" *> MP.space *> toEol)
    let fileOrDir = (Left <$> dir) <|> (Right <$> file) 
    files <- snd . partitionEithers <$> many (fileOrDir) 
    subdirs <- many (MP.try parser) 
    _ <- MP.optional "$ cd ..\n"
    return $ Dir name subdirs files

part1 :: Dir (Integer, Text) -> Sum Integer
part1 d = let s = foldMap (Sum . fst) d 
              subs = foldMap part1 . dirSubdirs $ d 
           in (if s < 100000 then s else 0) <> subs


findSmallestBigFile :: Integer -> Dir (Integer, Text) -> Maybe (Min Integer)
findSmallestBigFile desired d = 
    let s = getSum . foldMap (Sum . fst) $ d 
        subs = foldMap (findSmallestBigFile desired) . dirSubdirs $ d 
     in (if s >= desired then (Just . Min $ s) else Nothing) <> subs

part2 :: Dir (Integer, Text) -> Maybe (Min Integer)
part2 d = let spaceUsed = getSum . foldMap (Sum . fst) $ d
              spaceFree = 70000000 - spaceUsed
              spaceNeeded = 30000000 - spaceFree
           in findSmallestBigFile spaceNeeded d 

main :: IO ()
main = withParser "data/day07.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)