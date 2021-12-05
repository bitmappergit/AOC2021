{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, BlockArguments #-}

module Day4 where

import Data.List
import Text.Parsec
import Data.Maybe
import Text.Parsec.String
import Control.Monad
import Data.Function

type Bingo = [[Int]]
type TimedBingo = (Int, [[Int]])
type Game = ([Int], [Bingo])

parseFile :: Parser Game 
parseFile = (,) <$> parseCalled <*> many parseBingo 
  where integer = read <$> many1 digit
        parseCalled = integer `sepBy` char ','
        parseRow = (spaces *> integer) `sepBy` many1 (char ' ')
        parseBingo = forM [1..5] \_ -> parseRow <* char '\n'

numberCost :: [Int] -> Int -> Int
numberCost called num = fromJust $ findIndex (== num) called

bingoCost :: [Int] -> Bingo -> TimedBingo
bingoCost called bingo = do
  let getCost = map $ maximum . map (numberCost called)
  let verticalCost = minimum $ getCost $ transpose bingo 
  let horizontalCost = minimum $ getCost bingo 
  (min horizontalCost verticalCost, bingo)

sortBingos :: [Int] -> [Bingo] -> [TimedBingo]
sortBingos called bingos = sortBy (on compare fst) $ map (bingoCost called) bingos

solve :: [Int] -> [Bingo] -> (Int, Int)
solve called bingos = do
  let results = sortBingos called bingos
  let (bestDistance, best) = head results
  let (worstDistance, worst) = last results
  let bestCalled = take (bestDistance + 1) called
  let bestNotCalled = concatMap (filter (`notElem` bestCalled)) best
  let worstCalled = take (worstDistance + 1) called
  let worstNotCalled = concatMap (filter (`notElem` worstCalled)) worst
  (last bestCalled * sum bestNotCalled, last worstCalled * sum worstNotCalled)

main :: IO ()
main = do
  Right (called, bingos) <- parseFromFile parseFile "Day4.txt"
  let (best, worst) = solve called bingos
  print best
  print worst

