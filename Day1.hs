import Data.Function

solve :: Int -> [Int] -> Int
solve size input =
  case input of
    _ : xs -> solve size xs & if sum (take size input) < sum (take size xs) then succ else id
    [] -> 0

main :: IO ()
main = do
  nums <- map read . lines <$> readFile "Day1.txt"
  print $ solve 1 nums
  print $ solve 3 nums
