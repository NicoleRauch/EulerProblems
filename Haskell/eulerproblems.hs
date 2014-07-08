module Main

where

-- Problem 16

power2 0 = 1
power2 n = 2 * power2 (n-1)

quersumme 0 = 0
quersumme x = (x `mod` 10) + quersumme (x `div` 10)

problem16 = quersumme (power2 1000)


-- Problem 17

number 0 = ""
number 1 = "one"
number 2 = "two"
number 3 = "three"
number 4 = "four"
number 5 = "five"
number 6 = "six"
number 7 = "seven"
number 8 = "eight"
number 9 = "nine"
number 10 = "ten"
number 11 = "eleven"
number 12 = "twelve"
number 13 = "thirteen"
number 14 = "fourteen"
number 15 = "fifteen"
number 16 = "sixteen"
number 17 = "seventeen"
number 18 = "eighteen"
number 19 = "nineteen"
number 20 = "twenty"
number 30 = "thirty"
number 40 = "forty"
number 50 = "fifty"
number 60 = "sixty"
number 70 = "seventy"
number 80 = "eighty"
number 90 = "ninety"
number 100 = "onehundred"
number 1000 = "onethousand"
number x = ""

convert x
 | x <= 20 = number x
 | x < 100 = (number ((x `div` 10) * 10)) ++ (number (x `mod` 10))
 | x == 1000 = number x
 | x `mod` 100 == 0 = number (x `div` 100) ++ "hundred"
 | x < 1000 = (number (x `div` 100)) ++ "hundredand" ++ (convert (x - ((x `div` 100)*100)))
 | otherwise = number x

toWords [] = ""
toWords (x:xs) = convert x ++ toWords xs

problem17 =  length (toWords [1..1000])

-- Problem 18

munch [t] [] = [t]
munch (t:ts) (b1:b2:bs)
 | b1 > b2 = (t+b1) : (munch ts (b2:bs))
 | otherwise = (t+b2) : (munch ts (b2:bs))
munch _ _ = []

munchLists [l] = [l]
munchLists (b:t:rest) = munchLists ((munch t b):rest)

problem18 = munchLists [[4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23], [63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [41, 41, 26, 56, 83, 40, 80, 70, 33], [99, 65, 4, 28, 6, 16, 70, 92], [88, 2, 77, 73, 7, 63, 67], [19, 1, 23, 75, 3, 34], [20, 4, 82, 47, 65], [18, 35, 87, 10], [17, 47, 82], [95, 64], [75]]

-- munchLists [[8, 5, 9, 3], [2, 4, 6], [7, 4], [3]]

-- Problem 25

powerN x 0 = 1
powerN x n = x * powerN x (n-1)



fiblist [] = fiblist ((2,1):(1,1):[])
fiblist [x] = fiblist ((2,1):(1,1):[])
fiblist ((t1,x):(t2,y):xs) 
 | length (show (x+y)) < 1000 = fiblist ((t1+1, x+y):(t1, x):[])
 | otherwise = ((t1+1,x+y):(t1,x):[]) 


problem25 = fiblist []

-- Problem 67





main = putStrLn (show problem18)
