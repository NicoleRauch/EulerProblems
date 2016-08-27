module One where

sum below = Prelude.sum $ filter (\i -> (i `rem` 3 == 0) || (i `rem` 5 == 0)) [1..(below-1)]

main = do
  putStrLn $ show $ One.sum 10
  putStrLn $ show $ One.sum 1000

