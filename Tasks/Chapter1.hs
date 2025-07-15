-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main1 :: IO ()
main1 = do
  putStrLn "HC1T1 - doubleThenIncrement 5:"
  print (doubleThenIncrement 5) -- Expected: 11


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r ^ 2

main2 :: IO ()
main2 = do
  putStrLn "HC1T2 - Area of circle with radius 3:"
  print (circleArea 3) -- Expected: ~28.27


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main3 :: IO ()
main3 = do
  putStrLn "HC1T3 - Is 20 greater than 18?"
  print (greaterThan18 20) -- Expected: True


-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers = map fst

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main4 :: IO ()
main4 = do
  let players = [("Alice", 50), ("Bob", 75), ("Charlie", 60), ("Dave", 90), ("Eve", 40)]
  putStrLn "HC1T4 - Top 3 players:"
  print (getTopThreePlayers players) -- Expected: ["Dave", "Bob", "Charlie"]


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

main5 :: IO ()
main5 = do
  putStrLn "HC1T5 - First 10 numbers of an infinite list:"
  print (take 10 infiniteNumbers) -- Expected: [1,2,3,...,10]


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main6 :: IO ()
main6 = do
  putStrLn "HC1T6 - Sum of 7 and 5:"
  print (addNumbers 7 5) -- Expected: 12


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

main7 :: IO ()
main7 = do
  putStrLn "HC1T7 - Convert 98.6°F to Celsius:"
  print (fToC 98.6) -- Expected: ~37.0


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main8 :: IO ()
main8 = do
  putStrLn "HC1T8 - Apply (*2) twice to 3:"
  print (applyTwice (*2) 3) -- Expected: 12
