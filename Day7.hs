import Helpers
import Text.Parsec
import Text.Parsec.String

part1, part2 :: [Int] -> Int
part1 input = minimum [sum [abs (a - b) | a <- input] | b <- [0..maximum input]]
part2 input = minimum [sum [sum [1..abs (a - b)] | a <- input] | b <- [0..maximum input]]

main :: IO ()
main = do
  Right input <- parseFromFile (commaList integer) "Day7.txt"
  print $ part1 input
  print $ part2 input
