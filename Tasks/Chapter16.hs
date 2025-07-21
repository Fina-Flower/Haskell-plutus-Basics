--HC16T1: Reverse a String
reverseString :: String -> String
reverseString = reverse
main :: IO ()
main = do
  putStrLn "Enter a string to reverse:"
  input <- getLine
  putStrLn ("Reversed: " ++ reverseString input)

--HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
main :: IO ()
main = do
  putStrLn "Enter a string:"
  str <- getLine
  putStrLn (if isPalindrome str then "It's a palindrome!" else "Not a palindrome.")

--HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let n = read input :: Integer
  print (factorial n)

--HC16T4: Filter Even Numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even
main :: IO ()
main = do
  putStrLn "Enter numbers separated by space:"
  input <- getLine
  let numbers = map read (words input) :: [Int]
  print (filterEvens numbers)

--HC16T5: Uppercase String
import Data.Char (toUpper)
toUppercase :: String -> String
toUppercase = map toUpper
main :: IO ()
main = do
  putStrLn "Enter a string:"
  input <- getLine
  putStrLn (toUppercase input)

--HC16T6: nth Fibonacci Number
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
main :: IO ()
main = do
  putStrLn "Enter n:"
  input <- getLine
  let n = read input :: Int
  print (fibonacci n)

--HC16T7: Element Existence in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem
main :: IO ()
main = do
  putStrLn "Enter a list (space-separated):"
  input <- getLine
  putStrLn "Enter the element to find:"
  el <- getLine
  let list = words input
  print (elementExists el list)

--HC16T8: Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs
main :: IO ()
main = do
  putStrLn "Enter numbers to sort:"
  input <- getLine
  let nums = map read (words input) :: [Int]
  print (insertionSort nums)

--HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise   = x : removeDuplicates xs
main :: IO ()
main = do
  putStrLn "Enter list elements:"
  input <- getLine
  let list = words input
  print (removeDuplicates list)

--HC16T10: Character Frequency in String
import qualified Data.Map as Map
import Data.Char (toLower)
charFrequency :: String -> [(Char, Int)]
charFrequency str = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- str]
main :: IO ()
main = do
  putStrLn "Enter a string:"
  input <- getLine
  print (charFrequency input)
