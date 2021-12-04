{-# LANGUAGE BlockArguments, LambdaCase, LiberalTypeSynonyms #-}

import Data.List
import Lens

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

getCommon :: [Bool] -> Ordering
getCommon input = compare (count id input) (count not input)

gamma :: [Ordering] -> Int
gamma = toDec . map (== GT)

epsilon :: [Ordering] -> Int
epsilon = toDec . map (== LT)

toBool :: Char -> Bool
toBool = \case '0' -> False; '1' -> True

toDec :: [Bool] -> Int
toDec = foldl (\a (i, v) -> a & set (bitAt i) v) 0 . zip [0..] . reverse

data Sub
  = Sub { _input :: [([Bool], [Bool])]
        , _common :: [Ordering]
        } deriving (Show, Eq)

input :: Mono Lens Sub [([Bool], [Bool])]
input = lens _input \o n -> o { _input = n }

common :: Mono Lens Sub [Ordering]
common = lens _common \o n -> o { _common = n }

whittle :: (Ordering -> Bool) -> Sub -> Sub
whittle sysCheck = genCommon . shiftProcessed . checkCriteria
  where checkCriteria calc =
          calc & over input do
            filter \(bit : _, _) -> do
              sysCheck (calc % common & head) == bit

        shiftProcessed calc =
          calc & over input do
             map \(bit : xs, ys) -> (xs, bit : ys)

        genCommon calc =
          calc & set common do
            calc % input & map getCommon . transpose . map fst

cut :: (Ordering -> Bool) -> Sub -> Int
cut f calc =
  case calc % input of
    [(l, r)] -> reverse r <> l & toDec
    other -> cut f (whittle f calc)

oxygen, co2 :: Sub -> Int
oxygen = cut (/= LT)
co2 = cut (== LT)

main = do
  input <- map (map toBool) <$> lines <$> readFile "Day3.txt"
  let commons = map getCommon $ transpose input 
  print $ gamma commons * epsilon commons
  let calc = Sub (zip input $ repeat []) commons
  print $ oxygen calc * co2 calc
