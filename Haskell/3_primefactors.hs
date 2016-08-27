module M3 where

import Primes

primefactors :: Int -> [Int]
primefactors x = factors x primes
  where
    factors x (p:ps)
      | x < p          = []
      | x `rem` p == 0 = p : (factors (x `div` p) ps)
      | otherwise      = factors x ps

main = do
  putStrLn $ show $ primefactors 13195
  putStrLn $ show $ primefactors 600851475143
