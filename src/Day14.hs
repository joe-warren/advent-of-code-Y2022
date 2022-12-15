{-# Language OverloadedStrings #-}
module Day14 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Data.Map as M
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Control.Lens
import Linear.V2

data Thing = Rock | Sand deriving (Show, Eq)

parser :: Parser (M.Map (V2 Int) Thing)
parser = 
    let vector = V2 <$> (MPL.decimal <* ",") <*> MPL.decimal
        row = vector `MP.sepBy` " -> "
        rangeV a b = [min a b .. max a b]
        range (V2 ax ay) (V2 bx by) = [V2 x y | x <- rangeV ax bx, y <- rangeV ay by]
        rowToPoints r = concat $ zipWith range r (tail r)
        pointsToMap = M.fromList . fmap (, Rock) 
    in pointsToMap . concat <$> ((rowToPoints <$> row) `MP.sepBy` MP.newline)

fillWithSand :: M.Map (V2 Int) Thing -> M.Map (V2 Int) Thing
fillWithSand m = dropAll m
    where  
        maxY = maximum (fmap (view _y) . M.keys $ m) 
        inBounds (V2 x y) = y <= maxY
        nextStep :: V2 Int -> [V2 Int]
        nextStep x = (+ x) <$> [V2 0 1, V2 (-1) 1, V2 1 1]
        dropSand :: M.Map (V2 Int) Thing -> Maybe (V2 Int)
        dropSand m = let go p = case filter ((== Nothing) . (`M.lookup` m)) $ nextStep p of
                                    [] -> Just p
                                    (np:_) -> if inBounds np then go np else Nothing
                      in go (V2 500 0) 
        dropAll m = maybe m (\i -> dropAll $ m <> M.singleton i Sand) $ dropSand m

                   
part1 :: M.Map (V2 Int) Thing -> Int
part1 = length . filter (== Sand) . toList . fillWithSand

fillWithSand2 :: M.Map (V2 Int) Thing -> M.Map (V2 Int) Thing
fillWithSand2 m = dropAll m
    where  
        maxY = maximum (fmap (view _y) . M.keys $ m) 
        inBounds (V2 x y) = y <= maxY + 1
        nextStep :: V2 Int -> [V2 Int]
        nextStep x = (+ x) <$> [V2 0 1, V2 (-1) 1, V2 1 1]
        dropSand :: M.Map (V2 Int) Thing -> V2 Int
        dropSand m = let go p = case filter ((== Nothing) . (`M.lookup` m)) $ nextStep p of
                                    [] -> p
                                    (np:_) -> if inBounds np then go np else p
                      in go (V2 500 0) 
        dropAll m = case dropSand m of
                        (V2 500 0) -> m 
                        x -> dropAll $ M.insert x Sand m
                    
part2 :: M.Map (V2 Int) Thing -> Int
part2 = (1 +) . length . filter (== Sand) . toList . fillWithSand2

main :: IO ()
main = withParser "data/day14.txt" parser $ \res -> do
            print (res)
            print (part1 res)
            print (part2 res)