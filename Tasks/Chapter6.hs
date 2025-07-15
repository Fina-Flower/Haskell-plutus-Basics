-- HC6T1: Factorial (Recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = do
  print (factorial 5)  -- Expected: 120
  print (factorial 0)  -- Expected: 1


-- HC6T2: Fibonacci (Recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = do
  print (fibonacci 0)  -- 0
  print (fibonacci 1)  -- 1
  print (fibonacci 7)  -- 13


-- HC6T3: Sum of elements using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

main3 :: IO ()
main3 = do
  print (sumList [1,2,3,4,5])  -- 15
  print (sumList [])          -- 0


-- HC6T4: Product of elements using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

main4 :: IO ()
main4 = do
  print (productList [1,2,3,4]) -- 24
  print (productList [])        -- 1


-- HC6T5: Reverse a list (recursive)
reverseList :: [a] -> [a]
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

main5 :: IO ()
main5 = do
  print (reverseList [1,2,3])  -- [3,2,1]
  print (reverseList "hello") -- "olleh"


-- HC6T6: Check if element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs)
  | e == x    = True
  | otherwise = elementExists e xs

main6 :: IO ()
main6 = do
  print (elementExists 3 [1,2,3,4]) -- True
  print (elementExists 5 [1,2,3,4]) -- False


-- HC6T7: List length (recursive)
listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

main7 :: IO ()
main7 = do
  print (listLength [1,2,3,4]) -- 4
  print (listLength [])        -- 0


-- HC6T8: Filter even numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

main8 :: IO ()
main8 = do
  print (filterEven [1..10]) -- [2,4,6,8,10]


-- HC6T9: Map implementation
mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

main9 :: IO ()
main9 = do
  print (mapList (+1) [1,2,3])  -- [2,3,4]
  print (mapList (*2) [4,5])    -- [8,10]


-- HC6T10: Digits of a number (recursive)
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main10 :: IO ()
main10 = do
  print (digits 1234)  -- [1,2,3,4]
  print (digits 7)     -- [7]
