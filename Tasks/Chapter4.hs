-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

main1 :: IO ()
main1 = do
  print (weatherReport "sunny")
  print (weatherReport "rainy")
  print (weatherReport "cloudy")
  print (weatherReport "snowy") -- Unknown


-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
  | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
  | otherwise = "Invalid day"

main2 :: IO ()
main2 = do
  print (dayType "Monday")
  print (dayType "Saturday")
  print (dayType "Funday")


-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89  = "Good job!"
  | n >= 50 && n <= 69  = "You passed."
  | n >= 0 && n <= 49   = "Better luck next time."
  | otherwise           = "Invalid grade"

main3 :: IO ()
main3 = do
  print (gradeComment 95)
  print (gradeComment 75)
  print (gradeComment 60)
  print (gradeComment 40)
  print (gradeComment 110)


-- HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! So cute!"
specialBirthday 18 = "You're an adult now!"
specialBirthday 21 = "Legal everywhere now!"
specialBirthday _  = "Happy Birthday!"

main4 :: IO ()
main4 = do
  print (specialBirthday 1)
  print (specialBirthday 18)
  print (specialBirthday 21)
  print (specialBirthday 30)


-- HC4T5 - Task 5: Add a Catch-All Pattern with Age
specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 1  = "First birthday! So cute!"
specialBirthdayWithAge 18 = "You're an adult now!"
specialBirthdayWithAge 21 = "Legal everywhere now!"
specialBirthdayWithAge age = "Happy Birthday! You are " ++ show age ++ " years old."

main5 :: IO ()
main5 = do
  print (specialBirthdayWithAge 5)
  print (specialBirthdayWithAge 18)
  print (specialBirthdayWithAge 21)


-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one item."
whatsInsideThisList [_, _]   = "The list has two items."
whatsInsideThisList _        = "The list has many items."

main6 :: IO ()
main6 = do
  print (whatsInsideThisList ([] :: [Int]))
  print (whatsInsideThisList [1])
  print (whatsInsideThisList [1, 2])
  print (whatsInsideThisList [1, 2, 3])


-- HC4T7 - Task 7: Ignore Elements in a List
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

main7 :: IO ()
main7 = do
  print (firstAndThird [10, 20, 30, 40]) -- [10,30]
  print (firstAndThird [1, 2])           -- []
  print (firstAndThird [5, 6, 7])        -- [5,7]


-- HC4T8 - Task 8: Extract Values from Tuples
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) =
  name ++ " is " ++ show age ++ " years old and " ++ status
  where status = if isStudent then "a student." else "not a student."

main8 :: IO ()
main8 = do
  print (describeTuple ("Alice", 22, True))
  print (describeTuple ("Bob", 30, False))
