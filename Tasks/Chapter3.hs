import Numeric (showHex)
import Text.Printf (printf)

-- HC3T1 - Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n =
  if n > 0 then "Positive"
  else if n < 0 then "Negative"
  else "Zero"

main1 :: IO ()
main1 = do
  print (checkNumber 5)
  print (checkNumber (-3))
  print (checkNumber 0)


-- HC3T2 - Determine grade based on score
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

main2 :: IO ()
main2 = do
  print (grade 95)
  print (grade 72)
  print (grade 50)


-- HC3T3 - Convert RGB to hex string
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let rHex = printf "%02X" r
      gHex = printf "%02X" g
      bHex = printf "%02X" b
  in rHex ++ gHex ++ bHex

main3 :: IO ()
main3 = do
  print (rgbToHex (255, 0, 127))  -- Expected: "FF007F"
  print (rgbToHex (0, 255, 64))   -- Expected: "00FF40"


-- HC3T4 - Triangle area using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

main4 :: IO ()
main4 = do
  print (triangleArea 3 4 5)   -- Expected: 6.0
  print (triangleArea 7 8 9)   -- ~26.83


-- HC3T5 - Determine triangle type
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

main5 :: IO ()
main5 = do
  print (triangleType 3 3 3)
  print (triangleType 5 5 8)
  print (triangleType 6 7 8)


-- HC3T6 - Check leap year
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

main6 :: IO ()
main6 = do
  print (isLeapYear 2000) -- True
  print (isLeapYear 1900) -- False
  print (isLeapYear 2024) -- True


-- HC3T7 - Determine season by month
season :: Int -> String
season m
  | m == 12 || m == 1 || m == 2 = "Winter"
  | m >= 3 && m <= 5 = "Spring"
  | m >= 6 && m <= 8 = "Summer"
  | m >= 9 && m <= 11 = "Autumn"
  | otherwise = "Invalid month"

main7 :: IO ()
main7 = do
  print (season 3)  -- Spring
  print (season 7)  -- Summer
  print (season 11) -- Autumn


-- HC3T8 - BMI category using `where`
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25   = "Normal"
  | bmi < 30   = "Overweight"
  | otherwise  = "Obese"
  where bmi = weight / (height ^ 2)

main8 :: IO ()
main8 = do
  print (bmiCategory 70 1.75) -- Normal
  print (bmiCategory 90 1.8)  -- Overweight


-- HC3T9 - Max of three numbers using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
  let max1 = max x y
      max2 = max max1 z
  in max2

main9 :: IO ()
main9 = do
  print (maxOfThree 10 20 15) -- 20
  print (maxOfThree 5 25 10)  -- 25


-- HC3T10 - Palindrome check using recursion and guards
isPalindrome :: String -> Bool
isPalindrome str
  | length str <= 1 = True
  | head str == last str = isPalindrome (init (tail str))
  | otherwise = False

main10 :: IO ()
main10 = do
  print (isPalindrome "racecar") -- True
  print (isPalindrome "haskell") -- False
  print (isPalindrome "madam")   -- True
