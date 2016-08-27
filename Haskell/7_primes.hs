module M7 where

import Primes

prime :: Int -> Int
prime num = last $ take num primes

main = do
  putStrLn $ show $ prime 6
  putStrLn $ show $ prime 10001
