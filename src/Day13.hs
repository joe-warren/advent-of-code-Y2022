{-# Language OverloadedStrings #-}
{-# Language DerivingVia #-}
module Day13 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Data.Fix
import Data.Functor.Classes
import Control.Applicative ((<|>))
import Data.Functor.Compose
import Data.Foldable (toList)
import Data.List (sortBy, elemIndex, singleton)

data Pair a = Pair a a deriving (Show, Functor, Foldable, Traversable)

newtype PacketF i a = PacketF {getPacketF :: [Either i a]} deriving (Show, Functor, Foldable, Traversable)
    deriving (Eq1, Show1) via (Compose [] (Either i))

packetParser :: Parser (Fix (PacketF Int))
packetParser = 
    let term = (Left <$> MPL.decimal) <|> (Right <$> packetParser)
     in Fix . PacketF <$> ("[" *> (term `MP.sepBy` ",") <* "]")


parser :: Parser [Pair (Fix (PacketF Int))] 
parser = (Pair <$> (packetParser <* MP.newline) <*> packetParser) `MP.sepBy` (MP.newline <* MP.newline)

{--
zipPackets :: Pair (Fix (PacketF Int)) -> a
zipPackets p = cotransverse f p
    where
        f (Pair (Fix (PacketF as)) (Fix (PacketF bs)) =   
--}
doCompare :: Pair (Fix (PacketF Int)) -> Ordering
doCompare (Pair (Fix (PacketF as)) (Fix (PacketF bs))) = foldMap compareOne (zip as bs) <> (length as `compare` length bs)
    where
        compareOne (Left a, Left b) = a `compare` b
        compareOne (Right ass, Right bss) = doCompare (Pair ass bss) 
        compareOne (Left a, Right bss) = doCompare $ Pair (Fix . PacketF $ [Left a]) bss
        compareOne (Right ass, Left b) = doCompare $ Pair ass (Fix . PacketF $ [Left b])


part1 :: [Pair (Fix (PacketF Int))] -> Int
part1 = sum . fmap fst . filter ((/= GT) . doCompare . snd). zip [1..]

doubleList :: Int -> Fix (PacketF Int)
doubleList = Fix . PacketF . singleton . Right . Fix . PacketF . singleton . Left  

part2 :: [Pair (Fix (PacketF Int))] -> Maybe Int
part2 p = 
    let dividerA = doubleList 2
        dividerB = doubleList 6
        compareTwo a b = doCompare (Pair a b) 
        allPackets = dividerA : dividerB : (concatMap toList $ p)
        sortedPackets = sortBy compareTwo allPackets
        index x = (+1) <$> elemIndex x sortedPackets 
    in (*) <$> index dividerA <*> index dividerB
main :: IO ()
main = withParser "data/day13.txt" parser $ \res -> do
            --print (res)
            --print (fmap doCompare $ res)
            print (part1 res)
            print (part2 res)


