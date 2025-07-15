import Data.Bool (bool)
import Data.List (intercalate)

-- HC2T1 - Task 1: Checking Types in GHCi
-- Expected types before checking in GHCi:
-- 42              :: Int
-- 3.14            :: Fractional => Double (default)
-- "Haskell"       :: String (which is [Char])
-- 'Z'             :: Char
-- True && False   :: Bool

main1 :: IO ()
main1 = do
  putStrLn "HC2T1 - Expected types before checking in GHCi:"
  putStrLn "42              :: Int"
  putStrLn "3.14            :: Fractional a => a (usually Double)"
  putStrLn "\"Haskell\"       :: String"
  putStrLn "'Z'             :: Char"
  putStrLn "True && False   :: Bool"

-- HC2T2 - Task 2: Function Type Signatures
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

main2 :: IO ()
main2 = do
  putStrLn "HC2T2 - Function Results:"
  print (add 3 4)                    -- Expected: 7
  print (isEven 6)                  -- Expected: True
  print (concatStrings "Hello, " "World!")  -- Expected: "Hello, World!"

-- HC2T3 - Task 3: Immutable Variables
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main3 :: IO ()
main3 = do
  putStrLn "HC2T3 - Immutable Variables:"
  print myAge
  print piValue
  print greeting
  print isHaskellFun
  putStrLn "Note: Trying to modify any of these will cause an error due to immutability."

-- HC2T4 - Task 4: Converting Between Infix and Prefix
main4 :: IO ()
main4 = do
  putStrLn "HC2T4 - Prefix and Infix Conversion:"
  -- Infix to Prefix
  print ((+) 5 3)         -- Expected: 8
  print ((*) 10 4)        -- Expected: 40
  print ((&&) True False) -- Expected: False
  -- Prefix to Infix
  print (7 + 2)           -- Expected: 9
  print (6 * 5)           -- Expected: 30
  print (True && False)   -- Expected: False

-- HC2T5 - Task 5: Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r ^ 2

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

main5 :: IO ()
main5 = do
  putStrLn "HC2T5 - Function Results:"
  print (circleArea 3)         -- Expected: ~28.27
  print (maxOfThree 5 9 2)     -- Expected: 9

-- HC2T6 - Task 6: Understanding Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

main6 :: IO ()
main6 = do
  putStrLn "HC2T6 - Int vs Integer:"
  print smallNumber
  print bigNumber
  putStrLn "Evaluating 2^64 :: Int in GHCi will cause overflow on 32-bit systems or return a negative number."

-- HC2T7 - Task 7: Boolean Expressions
boolAnd :: Bool
boolAnd = True && True -- Should evaluate to True

boolOr :: Bool
boolOr = False || False -- Should evaluate to False

boolNot :: Bool
boolNot = not False -- Should evaluate to True

comparisonFalse :: Bool
comparisonFalse = 5 > 10 -- Should evaluate to False

main7 :: IO ()
main7 = do
  putStrLn "HC2T7 - Boolean Expressions:"
  print boolAnd        -- Expected: True
  print boolOr         -- Expected: False
  print boolNot        -- Expected: True
  print comparisonFalse-- Expected: False
