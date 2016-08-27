module Two where

fibinf :: [Int] -> [Int]
fibinf (fn1:fn2:fns) = (fn1 + fn2) : fibinf ((fn1+fn2):fn1:fn2:fns)

fiblist :: [Int]
fiblist = 1 : 2 : fibinf [2, 1]

sum atmost = Prelude.sum $ takeWhile (\i -> i <= atmost) $ filter (\i -> even i) fiblist

main = do
  putStrLn $ show $ take 10 fiblist
  putStrLn $ show $ Two.sum 4000000

