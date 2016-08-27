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


main = do
  putStrLn $ show $ head $ multiple 10
  putStrLn $ show $ head $ multiple 20

