{-# Language OverloadedStrings #-}
module Day12 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.Graph.Inductive (LNode, empty, mkGraph, emap, labNodes)
import Data.Char (ord)
import Data.Distributive (distribute, collect)
import Control.Applicative ((<|>))
import Control.Monad (join)
import qualified Data.Map as M 
import Data.Maybe (catMaybes)
import Data.Graph.Inductive.Query.SP (spLength)

data Node = Node Char (Int, Int) deriving Show

isStart :: Node -> Bool
isStart (Node c _) = c == 'S'

isEnd :: Node -> Bool
isEnd (Node c _) = c == 'E'

nodeX :: Node -> Int
nodeX (Node _ (x, _)) = x

nodeY :: Node -> Int
nodeY (Node _ (_, y)) = y

nodeHeight :: Node -> Char
nodeHeight (Node c _) = c 

connected :: Char -> Char -> Bool
connected 'S' x = connected 'a' x
connected x 'S' = connected  x 'a'
connected 'E' x = connected 'z' x
connected x 'E' = connected  x 'z'
connected x y = (ord y - ord x) <= 1 

calcEdges :: [LNode Node] -> [(Int, Int, ())]
calcEdges nodes = 
    let maxX = maximum . fmap (nodeX . snd) $ nodes
        maxY = maximum . fmap (nodeY . snd) $ nodes
        m = M.fromList $ fmap (\x@(_, Node _ p) -> (p, x)) nodes
        testConnection (ia, Node a _) (ib, Node b _) = if connected a b then Just (ia, ib, ()) else Nothing
        pointsConnected :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int, ())
        pointsConnected a b = join $ testConnection <$> M.lookup a m <*> M.lookup b m
    in  catMaybes . concat $ 
                    ([[pointsConnected (x, y) (x, y+1), pointsConnected (x, y+1) (x, y)] | y <- [0.. maxY -1] , x <- [0..maxX]] ++ 
                     [[pointsConnected (x, y) (x+1, y), pointsConnected (x+1, y) (x, y)] | y <- [0.. maxY] , x <- [0..maxX-1]])

parser :: Parser (LNode Node, LNode Node, Gr Node ())
parser = do
    charGrid <- (MP.many (MP.char 'S' <|> MP.char 'E' <|> MP.lowerChar)) `MP.sepBy` "\n"
    let unlabeledNodes = fmap (\(x, (y, c)) -> Node c (x, y)) . concat . zipWith (\i w -> (,) i <$> w) [0..] . fmap (zip [0..]) $ charGrid
    let labeledNodes = zip [0..] unlabeledNodes 
    [start] <- pure $ filter (isStart . snd) labeledNodes
    [end] <- pure $ filter (isEnd . snd) labeledNodes
    let edges = calcEdges labeledNodes
    return (start, end, mkGraph labeledNodes edges)

part1 :: (LNode Node, LNode Node, Gr Node ()) -> Maybe Int
part1 ((s, _), (e, _), gr) = spLength s e (emap (const 1) gr)

part2 :: (LNode Node, LNode Node, Gr Node ()) -> Int
part2 (_, (e, _), gr) = 
    let spos = fmap fst . filter ((== 'a') . nodeHeight . snd) . labNodes $ gr
     in minimum . catMaybes $ (\s -> spLength s e (emap (const 1) gr)) <$> spos

main :: IO ()
main = withParser "data/day12.txt" parser $ \res -> do
            --print (res)
            print (part1 res)
            print (part2 res)