import Control.Monad 

import Criterion
import Criterion.Main

import Data.List

import Lib

import System.Random

infixl 1 <&>
x <&> f = f <$> x

main :: IO ()
main = do
  let randomsN k = replicateM (2^k) randomIO :: IO [Int]
  defaultMain $ [1 .. 20] <&> \k -> env (randomsN k) $ \xs -> bgroup ("2^" ++ show k ++ " items")
    [
      bench "Data.sort" (nf sort xs),
      bench "mergeSort" (nf mergeSort xs)
    ]

