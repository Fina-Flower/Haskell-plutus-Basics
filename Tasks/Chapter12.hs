--HC12T1: Print a Welcome Message
main1 :: IO ()
main1 = putStrLn "Welcome to Haskell Programming!"

--HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y
main2 :: IO ()
main2 = print (addTwoNumbers 5 7)

--HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
main3 :: IO ()
main3 = print (factorial 5)

--HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
main4 :: IO ()
main4 = print [fibonacci n | n <- [0..9]]

--HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = cleaned == reverse cleaned
  where cleaned = filter (/= ' ') (map toLower str)
main5 :: IO ()
main5 = do
  putStrLn "Enter a string:"
  input <- getLine
  print (isPalindrome input)

--HC12T6: Sort a List of Integers
import Data.List (sort)
main6 :: IO ()
main6 = do
  putStrLn "Enter numbers separated by spaces:"
  input <- getLine
  let numbers = map read (words input) :: [Int]
  print (sort numbers)

--HC12T7: Calculate Circle Area
calculateCircleArea :: Float -> Float
calculateCircleArea r = pi * r * r
main7 :: IO ()
main7 = print (calculateCircleArea 5)

--HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys
main8 :: IO ()
main8 = print (mergeLists [1,3,5] [2,4,6])

--HC12T9: Read and Print File Content
import System.IO
import System.Directory (doesFileExist)
main9 :: IO ()
main9 = do
  putStrLn "Enter file name:"
  fileName <- getLine
  exists <- doesFileExist fileName
  if exists
    then readFile fileName >>= putStrLn
    else putStrLn "File does not exist."

--HC12T10: Mathematical Operations Module
module MathOperations (add, subtract', multiply, divide) where
add :: Int -> Int -> Int
add x y = x + y
subtract' :: Int -> Int -> Int
subtract' x y = x - y
multiply :: Int -> Int -> Int
multiply x y = x * y
divide :: Int -> Int -> Maybe Float
divide _ 0 = Nothing
divide x y = Just (fromIntegral x / fromIntegral y)
