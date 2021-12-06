import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Data.List

accum :: [Int] -> [(Int, Int)]
accum input = [(fish, length $ filter (== fish) input) | fish <- nub $ sort input]

sumFish :: [(Int, Int)] -> [(Int, Int)]
sumFish input = [(fish, sum $ map snd $ filter ((== fish) . fst) input) | fish <- nub $ map fst input]

stepSum :: [(Int, Int)] -> [(Int, Int)]
stepSum = sumFish . step
  where step [] = []
        step ((timer, amount) : rest) =
          case timer of
            0 -> (8, amount) : (6, amount) : step rest
            n -> (n - 1, amount) : step rest

integer :: Parser Int
integer = read <$> many1 digit

getIterationSize :: [(Int, Int)] -> Int -> Int
getIterationSize input n = sum $ map snd $ iterate stepSum input !! n
  
main :: IO ()
main = do
  Right input <- parseFromFile (sepBy1 integer (char ',')) "Day6.txt"
  let accumulated = accum input
  print $ getIterationSize accumulated 80
  print $ getIterationSize accumulated 256
