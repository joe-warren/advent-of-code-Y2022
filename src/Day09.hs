{-# Language OverloadedStrings #-}
module Day09 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Tuple (swap)
import Linear.V2
import Linear.Affine
import Linear.Vector (zero) 
import qualified Data.Set as S
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE 

-- give these silly names to avoid clashing with Either
data Dir = Uppp | Down | Leff | Righ deriving Show

parser :: Parser [(Dir, Int)]
parser = 
    let d = ("U" $> Uppp) <|> ("D" $> Down) <|> ("L" $> Leff) <|> ("R" $> Righ)
     in ((,) <$> d <* " " <*> MPL.decimal) `MP.sepBy` "\n"

flatten :: [(Dir, Int)] -> [Dir]
flatten = concat . fmap ( uncurry replicate . swap) 

doMove :: Num a => Dir -> V2 a -> V2 a
doMove Uppp = (+ V2 0 1) 
doMove Down = (.-. (V2 0 1))
doMove Leff = (.-. (V2 1 0))
doMove Righ = (+ V2 1 0) 

step :: Dir -> (V2 Int, V2 Int) -> (V2 Int, V2 Int)
step d (h, t) = let nh = doMove d h
                    delta = if qdA nh t > 2 then signum <$> (nh - t) else V2 0 0
                    nt = t + delta
                 in (nh, nt)

part1 :: [(Dir, Int)] -> Int
part1 =  length . S.fromList . fmap snd . scanl (flip step) (V2 0 0, V2 0 0) . flatten 

step2 :: Dir -> NE.NonEmpty (V2 Int) -> NE.NonEmpty (V2 Int)
step2 d (h :| t) =
    let go nh (t:ts) = let delta = if qdA nh t > 2 then signum <$> (nh - t) else V2 0 0
                           nt = t + delta
                           in nt : go nt ts
        go _ [] = [] 
        nh = doMove d h
        in nh :| go nh t

part2 :: [(Dir, Int)] -> Int
part2 =  length . S.fromList . fmap NE.last . scanl (flip step2) (zero :| replicate 9 zero) . flatten 

main :: IO ()
main = withParser "data/day09.txt" parser $ \res -> do
            print (part1 res)
            print (part2 res)