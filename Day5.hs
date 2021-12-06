{-# LANGUAGE BlockArguments, ParallelListComp #-}

import Optics
import Data.Function
import Text.Parsec
import Text.Parsec.String
import Control.Arrow
import Data.List

type Point a = (a, a)
type Path a = (Point a, Point a)

integer :: Parser Int
integer = read <$> many1 digit

parsePoint :: Parser (Point Int)
parsePoint = do
  a <- integer
  char ','
  b <- integer
  pure (a, b)

parsePath :: Parser (Path Int)
parsePath = do
  a <- parsePoint
  between space space (string "->")
  b <- parsePoint
  pure (a, b)

parseFile :: Parser [Path Int]
parseFile = sepEndBy1 parsePath spaces

range :: Int -> Int -> [Int]
range a b =
  case compare a b of
    GT -> [a, pred a .. b]
    LT -> [a .. b]
    EQ -> [a]

frequency :: Ord a => [a] -> [(Int, a)] 
frequency = map (length &&& head) . group . sort

filterHV :: [Path Int] -> [Path Int]
filterHV = filter \((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2

calculatePoints :: Path Int -> [Point Int]
calculatePoints ((x1, y1), (x2, y2)) = do
  let l@(x : xs) = range x1 x2
  let r@(y : ys) = range y1 y2
  case (xs, ys) of
    ([], []) -> zip (repeat x) (repeat y)
    ([], _ ) -> zip (repeat x) r 
    ( _, []) -> zip l (repeat y)
    ( _, _ ) -> zip l r

main :: IO ()
main = do
  Right paths <- parseFromFile parseFile "Day5.txt"
  let pointsHV = concatMap calculatePoints $ filterHV paths
  let points = concatMap calculatePoints paths
  let overlappedPoints = length . filter (> 1) . map fst . frequency
  print $ overlappedPoints pointsHV
  print $ overlappedPoints points
