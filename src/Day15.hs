{-# Language OverloadedStrings #-}
{-# Language DeriveFunctor #-}
module Day15 (main) where

import CommonParsingStuff 
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Linear.V2 
import Control.Lens
import Control.Monad (join)
import Data.List (sort, singleton)
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)


data Observation = Observation {observationSensor :: V2 Int, observationBeacon :: V2 Int } deriving Show

parser :: Parser [Observation]
parser = 
    let signedParser = MPL.signed MP.space MPL.decimal
        vector = V2 <$> ("x=" *> signedParser) <*> (", y=" *> signedParser)
        line = Observation <$> (("Sensor at " *> vector) <* ": closest beacon is at ") <*> vector
     in line `MP.sepBy` MP.newline

newtype Spans a = Spans {getSpans :: [(a, a)]} deriving Functor

instance Ord a => Semigroup (Spans a) where
    (<>) (Spans a) (Spans b) = Spans . merge . sort $ (a <> b)
        where merge ((s1, e1):(s2, e2):xs) | e1 >= e2 = merge ((s1, e1):xs)
                                           | e1 >= s2 = merge ((s1, e2): xs)
                                           | otherwise = (s1, e1) : merge ((s2, e2): xs)
              merge x = x

instance Ord a => Monoid (Spans a) where
    mempty = Spans []

emptyRegion :: Int -> Observation -> Spans Int
emptyRegion rowId (Observation (s@(V2 sx sy)) (b@(V2 bx by))) = 
    let d = sum . fmap abs $ (s - b)
        dy = abs (rowId - sy)
        dx = d - dy 
    in if dy >= d 
            then mempty 
                else Spans [(sx - dx, sx + dx)]

spanSize :: Num a => Spans a -> a
spanSize (Spans xs) = let f (lo, hi) = hi - lo
                        in sum $ f <$> xs

part1 :: Int -> [Observation] -> Int
part1 rowId = spanSize . foldMap (emptyRegion rowId)

tuningFrequency :: V2 Int -> Int
tuningFrequency (V2 x y) = x * 4000000 + y 

beaconSpans :: Int -> [Observation] -> Spans Int
beaconSpans y = foldMap (Spans . singleton . join (,) . view _x) . filter  ((== y) . view _y) . (fmap observationBeacon)

emptySpace :: Spans Int -> Maybe Int
emptySpace (Spans [(_, x), (x2, _)]) | x + 1 < x2  = Just (x + 1)  
emptySpace _ = Nothing

part2 :: Int -> [Observation] -> [(V2 Int, Int)]
part2 maxSz os = fmap (id &&& tuningFrequency) . catMaybes $ [ fmap (`V2` y) . emptySpace . (beaconSpans y <> foldMap (emptyRegion y)) $ os | y <- [0..maxSz]]  

main :: IO ()
main = withParser "data/day15.txt" parser $ \res -> do
            print (res)
            print (part1 2000000 res)
            print (part2 4000000 res)