module Lib (mergeSort, merge) where

mergeSort :: (Ord a) => [a] -> [a]
mergeSort = reiterate mergeWindows . map return

reiterate :: ([[a]] -> [[a]]) -> ([[a]] -> [a])
reiterate f [xs] = xs
reiterate f xss = reiterate f $! f xss

mergeWindows :: (Ord a) => [[a]] -> [[a]]
mergeWindows (w1 : w2 : rest) = w1' `seq` wRest `seq` w1' : wRest where
  w1' = merge w1 w2 
  wRest = mergeWindows rest
mergeWindows ws = ws

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs1@(x1 : rest1) xs2@(x2 : rest2)
  | x1 <= x2 = (x1 :) $! merge rest1 xs2
  | otherwise = (x2 :) $! merge xs1 rest2
merge [] xs2 = xs2
merge xs1 [] = xs1
