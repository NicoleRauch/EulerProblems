module M6 where

diff :: Int -> Int
diff maxnum = ((sum [1..maxnum])^2) - (sum $ map (^ 2) [1..maxnum])

main = do
  putStrLn $ show $ M6.diff 10
  putStrLn $ show $ M6.diff 100
