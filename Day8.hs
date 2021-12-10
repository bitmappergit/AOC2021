{-# LANGUAGE BlockArguments, LiberalTypeSynonyms, LambdaCase #-}

import Data.List
import Data.Maybe
import Data.Function
import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Control.Monad

part1 :: [String] -> Int
part1 = sum . map \a -> if elem (length a) [2, 3, 4, 7] then 1 else 0

part2 :: [([String], [String])] -> Int
part2 = sum . map read . map (concat . map show) . map \(input, output) -> do
  let one = fromJust $ find ((== 2) . length) input 
  let four = fromJust $ find ((== 4) . length) input 
  let topL = four \\ one
  let same x y = all (`elem` y) x && all (`elem` x) y
  output & map \x ->
    case length x of
      2 -> 1
      3 -> 7
      4 -> 4
      5 | same (union topL x) x -> 5
        | same (union one x) x -> 3
        | otherwise -> 2
      6  | same (union four x) x -> 9
         | same (union topL x) x -> 6
         | otherwise -> 0
      7 -> 8

parseSignals :: Parser [String]
parseSignals = many1 (oneOf "abcdefg") `sepEndBy1` many1 (char ' ') 

parseLine :: Parser ([String], [String])
parseLine = do
  inputs <- parseSignals
  string "| "
  outputs <- parseSignals
  pure (inputs, outputs)

