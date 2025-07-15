-- HC5T1: Apply a function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

main1 :: IO ()
main1 = do
  print (applyThrice (+2) 0)  -- Expected: 6
  print (applyThrice (*2) 1)  -- Expected: 8


-- HC5T2: Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

main2 :: IO ()
main2 = print oddNumbers  -- Expected: [1,3,5,...,29]


-- HC5T3: Check for any word starting with uppercase
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

main3 :: IO ()
main3 = do
  print (hasUppercaseStart ["hello", "world"])     -- False
  print (hasUppercaseStart ["hello", "World"])     -- True
  print (hasUppercaseStart ["", ""])               -- False


-- HC5T4: Rewrite using lambda function
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main4 :: IO ()
main4 = do
  print (biggerThan10 5)   -- False
  print (biggerThan10 15)  -- True


-- HC5T5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*5)

main5 :: IO ()
main5 = do
  print (multiplyByFive 3)   -- 15
  print (multiplyByFive 10)  -- 50


-- HC5T6: Function composition to square and filter even results
squareEven :: [Int] -> [Int]
squareEven = filter even . map (^2)

main6 :: IO ()
main6 = do
  print (squareEven [1..10]) -- Expected: squares like 4, 16, 36, 64, 100


-- HC5T7: Use $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main7 :: IO ()
main7 = print result  -- Expected: 2×4 + 2×5 + ... + 2×10 = 2×(4+5+6+7+8+9+10) = 2×49 = 98


-- HC5T8: Point-free style
addFive :: Int -> Int
addFive = (+5)

main8 :: IO ()
main8 = do
  print (addFive 10)  -- 15
  print (addFive 0)   -- 5


-- HC5T9: Apply a function twice to every element in a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

main9 :: IO ()
main9 = do
  print (transformList (+1) [1,2,3]) -- [3,4,5]
  print (transformList (*2) [1,2,3]) -- [4,8,12]


-- HC5T10: Check if any square in a list is greater than 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2) . filter (>0)

main10 :: IO ()
main10 = do
  print (anySquareGreaterThan50 [1,2,3,4,5,6,7])  -- False
  print (anySquareGreaterThan50 [1,8,3])          -- True (8^2 = 64)
