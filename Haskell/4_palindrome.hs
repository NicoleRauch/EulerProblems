module M4 where

palindrome :: Int -> [Int]
palindrome digits = palinMinMax (10^(digits-1)) ((10^digits)-1)

palinMinMax :: Int -> Int -> [Int]
palinMinMax min max = [ f1*f2 | f1 <- [min..max], f2 <- [min..max], show (f1*f2) == reverse (show (f1*f2))]


main = do
  putStrLn $ show $ maximum $ palindrome 2
  putStrLn $ show $ maximum $ palindrome 3

