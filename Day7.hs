{-# LANGUAGE BlockArguments, ParallelListComp #-}

import Data.List
import Data.Function
import Text.Parsec
import Text.Parsec.String

integer :: Parser Int
integer = read <$> many1 digit

sample :: Num a => [a]
sample = [16,1,2,0,4,2,7,1,2,14]

part1, part2 :: [Int] -> Int
part1 input = minimum [sum [abs (a - b) | a <- input] | b <- [0..maximum input]]
part2 input = minimum [sum [sum [1..abs (a - b)] | a <- input] | b <- [0..maximum input]]

main :: IO ()
main = do
  Right input <- parseFromFile (sepBy1 integer (char ',')) "Day7.txt"
  print $ part1 input
  print $ part2 input
