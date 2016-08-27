module M5 where

import Primes
import Data.List ((\\))

-- works but is rather slow:
multiple :: Int -> [Int]
multiple maxnum =
  let ps = (takeWhile (<= maxnum) primes)
      nonps = [1..maxnum] \\ ps
      candidates = filter (\x -> all (\i -> x `rem` i == 0) ps) [1..]
  in [x | x <- candidates, all (\i -> x `rem` i == 0) nonps]


multifactors :: Int -> [Int]
multifactors x = factors x primes
  where
    factors x (p:ps)
      | x < p          = []
      | x `rem` p == 0 = p : (factors (x `div` p) (p:ps))
      | otherwise      = factors x ps


filterFactors [] fs = fs
filterFactors (x:xs) fs =
  let addUnique oldL newL = oldL ++ (newL \\ oldL)
  in filterFactors [y | y <- xs, x `rem` y /= 0] $ addUnique fs (multifactors x)

-- try a different strategy:
multiple2 :: Int -> Int
multiple2 maxnum =
  let numbers = filterFactors [maxnum,(maxnum-1)..1] []
  in foldr (\i j -> i * j) 1 numbers

main = do
  putStrLn $ show $ head $ multiple 10
  putStrLn $ show $ multiple2 20
  -- putStrLn $ show $ head $ multiple 20

