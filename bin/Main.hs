{-# LANGUAGE BangPatterns #-}
import Control.DeepSeq
import Data.Maybe

import Lib

import System.Environment
import System.Random

main :: IO ()
main = do
  args <- getArgs
  let n = maybe 0 read $ listToMaybe args :: Int
      g = mkStdGen n
      xs = take (2^n) $ randoms g :: [Int]
      !xs' = force $ mergeSort $!! xs
  return ()

