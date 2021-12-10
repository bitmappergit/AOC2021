{-# LANGUAGE BlockArguments, LambdaCase, LiberalTypeSynonyms, OverloadedLists, MonadComprehensions #-}

import Optics
import Data.Sequence as Seq

readHeightmap :: String -> Seq (Seq Int)
readHeightmap = fmap (fmap (read . (: []))) . Seq.fromList . map Seq.fromList . lines

atXY :: Int -> Int -> Simple Lens (Seq (Seq a)) a
atXY x y = at y . at x

small :: Seq (Seq Int)
small = [ [2,1,9,9,9,4,3,2,1,0]
        , [3,9,8,7,8,9,4,9,2,1]
        , [9,8,5,6,7,8,9,8,9,2]
        , [8,7,6,7,8,9,6,7,8,9]
        , [9,8,9,9,9,6,5,6,7,8]
        ]

dimensions :: Seq (Seq Int) -> (Int, Int)
dimensions m = (Seq.length (Seq.index m 0), Seq.length m)

adjacent :: Seq (Seq Int) -> Int -> Int -> [Int]
adjacent m x y =
  let (xdim, ydim) = dimensions m
   in case (x, y) of
        (0, 0) -> [m % atXY 0 1, m % atXY 1 0]
        (0, _) -> let always = [m % atXY 0 (y - 1), m % atXY 1 y]
                      yFunc = if y < ydim - 1 then ((m % atXY 0 (y + 1)) :) else id
                   in yFunc always
        (_, 0) -> let always = [m % atXY (x - 1) 0, m % atXY x 1]
                      xFunc = if x < xdim - 1 then ((m % atXY (x + 1) 0) :) else id
                   in xFunc always
        (_, _) -> let always = [m % atXY (x - 1) y, m % atXY x (y - 1)]
                      xFunc = if x < xdim - 1 then ((m % atXY (x + 1) y) :) else id
                      yFunc = if y < ydim - 1 then ((m % atXY x (y + 1)) :) else id
                      xyFunc = xFunc . yFunc
                   in xyFunc always
getLow :: Seq (Seq Int) -> Int -> Int -> Maybe Int
getLow m x y =
  let v = m % atXY x y
   in if and [v < i | i <- adjacent m x y]
      then Just (v + 1)
      else Nothing
