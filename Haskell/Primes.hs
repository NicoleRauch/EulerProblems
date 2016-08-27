module Primes
  ( primes )
  where

primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve [ y | y <- xs, y `rem` x /= 0]

main = do
  putStrLn $ show $ take 20 primes

