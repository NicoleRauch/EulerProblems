module M5 where

multiple :: Int -> [Int]
multiple maxnum = [x | x <- [1..], all (\i -> x `rem` i == 0) [1..maxnum]]

main = do
  putStrLn $ show $ head $ multiple 10
  putStrLn $ show $ head $ multiple 20

