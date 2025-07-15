-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount = "From: " ++ from ++ ", To: " ++ to ++ ", Amount: " ++ show amount

main1 :: IO ()
main1 = print (generateTx "addr1" "addr2" 100)


-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 12) Cash

main2 :: IO ()
main2 = print bob


-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main3 :: IO ()
main3 = do
  print (area (Circle 5))      -- ~78.54
  print (area (Rectangle 10 5))-- 50


-- HC8T4: Record Syntax for Employee
data Employee = Employee
  { name :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

main4 :: IO ()
main4 = print richard


-- HC8T5: Record Syntax for Person
data PersonRecord = PersonRecord
  { personName :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonRecord
person1 = PersonRecord "Alice" 30 True

person2 :: PersonRecord
person2 = PersonRecord "Bob" 22 False

main5 :: IO ()
main5 = do
  print person1
  print person2


-- HC8T6: Record Syntax for Shape Variants
data ShapeVariant =
    CircleShape { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleShape { width :: Float, height :: Float, color :: String }
  deriving Show

circle1 :: ShapeVariant
circle1 = CircleShape (0, 0) "Red" 5

rectangle1 :: ShapeVariant
rectangle1 = RectangleShape 10 5 "Blue"

main6 :: IO ()
main6 = do
  print circle1
  print rectangle1


-- HC8T7: Describing Animals
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = name ++ " is a dog."
describeAnimal (Cat name) = name ++ " is a cat."

dog1 :: Animal
dog1 = Dog "Max"

cat1 :: Animal
cat1 = Cat "Luna"

main7 :: IO ()
main7 = do
  print (describeAnimal dog1)
  print (describeAnimal cat1)


-- HC8T8: Type Synonyms and Greeting
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

main8 :: IO ()
main8 = print (greet "Charlie" 25)


-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
  let tx = Transaction fromAddr toAddr amt ("tx-" ++ show amt ++ "-" ++ take 4 toAddr)
  in transactionId tx

main9 :: IO ()
main9 = print (createTransaction "addr1" "addrXYZ" 200)


-- HC8T10: Deriving Show for Book
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving Show

book1 :: Book
book1 = Book "The Haskell Way" "Ada Lovelace" 2025

main10 :: IO ()
main10 = print book1
