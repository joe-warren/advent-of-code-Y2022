{-# Language OverloadedStrings #-}
module Day11 (main) where

import CommonParsingStuff
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
import Control.Applicative ((<|>))
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Monoid (Endo (..), Sum (..), Product (..))
import Data.Ord (Down (..))
import Data.List (sortOn, genericLength)
import Data.Foldable (foldlM, toList)

newtype MonkeyId = MonkeyId Int deriving (Eq, Ord, Show)

data Expr = EMul Expr Expr | EAdd Expr Expr | EVal Integer | EOld deriving Show

eval :: Expr -> Integer -> Integer
eval (EAdd a b) i = (eval a i) + (eval b i)
eval (EMul a b) i = (eval a i) * (eval b i)
eval (EVal x) _ = x
eval EOld i = i 

data Monkey = Monkey {
        monkeyItems :: [Integer],
        monkeyOperation :: Expr,
        monkeyTest :: Integer,
        monkeyTrue :: MonkeyId,
        monkeyFalse :: MonkeyId
    } deriving Show

exprParser :: Parser Expr
exprParser = 
    let binary  name f = InfixL  (f <$ name)
    in makeExprParser 
        (EOld <$ "old"  <|> EVal <$> MPL.decimal)
        [[binary " * " EMul],[binary " + " EAdd]]

monkeyParser :: Parser (MonkeyId, Monkey)
monkeyParser = do
    monkeyId <- MonkeyId <$> ("Monkey " *> MPL.decimal <* ":\n")
    startingItems <- reverse <$> ("  Starting items: " *> (MPL.decimal `MP.sepBy` ", ") <* "\n")
    operation <- "  Operation: new = " *> exprParser <* "\n"
    test <- "  Test: divisible by " *> MPL.decimal <* "\n"
    ifTrue <- MonkeyId <$> ("    If true: throw to monkey " *> MPL.decimal <* "\n")
    ifFalse <- MonkeyId <$> ("    If false: throw to monkey " *> MPL.decimal <* "\n")
    return (monkeyId, Monkey startingItems operation test ifTrue ifFalse)

parser :: Parser (M.Map MonkeyId Monkey)
parser = M.fromList <$> (monkeyParser `MP.sepBy` "\n")

oneThrow :: (Integer -> Integer) -> Monkey -> Integer -> (MonkeyId, Integer)
oneThrow f m i = 
    let newI = f . (eval $ monkeyOperation m) $ i
    in if newI `mod` monkeyTest m == 0
         then (monkeyTrue m, newI)
         else (monkeyFalse m, newI)

addItem :: Integer -> Monkey -> Monkey
addItem i m@(Monkey {monkeyItems = items}) = m {monkeyItems = i:items}

addItemById :: (MonkeyId, Integer) -> M.Map MonkeyId Monkey -> M.Map MonkeyId Monkey
addItemById (mid, item) = M.adjust (addItem item) mid

zeroItemsById :: MonkeyId -> M.Map MonkeyId Monkey -> M.Map MonkeyId Monkey
zeroItemsById = M.adjust (\monkey -> monkey {monkeyItems = []}) 

oneMonkey :: (Integer -> Integer) -> MonkeyId -> M.Map MonkeyId Monkey -> (MM.MonoidalMap MonkeyId (Sum Integer), M.Map MonkeyId Monkey)
oneMonkey f mid mmap = case M.lookup mid mmap of 
    Nothing -> (mempty, mmap)
    Just m-> ( MM.singleton mid (Sum . genericLength . monkeyItems $ m)
             , zeroItemsById mid $ (appEndo . mconcat $ Endo . addItemById . oneThrow f m <$> monkeyItems m) mmap) 
    
allMonkeys :: (Integer ->Integer) -> M.Map MonkeyId Monkey -> (MM.MonoidalMap MonkeyId (Sum Integer), M.Map MonkeyId Monkey)
allMonkeys f m = foldlM (flip (oneMonkey f)) m (M.keys m)

part1 :: M.Map MonkeyId Monkey -> Integer
part1 m = let (counts, _) = (iterate (>>= allMonkeys (`div` 3)) (pure m)) !! 20
              (max1:max2:_) = fmap getSum . sortOn (Down) . toList $ counts
            in max1 * max2 

part2 :: M.Map MonkeyId Monkey -> Integer
part2 m = let 
              commonMultiple = getProduct $ foldMap (Product . monkeyTest) m
              (counts, _) = (iterate (>>= allMonkeys (`mod` commonMultiple)) (pure m)) !! 10000
              (max1:max2:_) = fmap getSum . sortOn (Down) . toList $ counts
            in max1 * max2 
            
main :: IO ()
main = withParser "data/day11.txt" parser $ \res -> do
            print (res)
            print (part1 res)
            print (part2 res)