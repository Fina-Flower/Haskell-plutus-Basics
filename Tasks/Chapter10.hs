-- HC10T1: ShowSimple Type Class
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash"
  showSimple Card = "Card"
  showSimple Cryptocurrency = "Crypto"

main1 = do
  print $ showSimple Cash
  print $ showSimple Card

-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

main2 = print $ sumUp [1, 2, 3, 4, 5]

-- HC10T3: Comparable Type Class
data Blockchain = Bitcoin | Ethereum | Cardano deriving (Show)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith Bitcoin Bitcoin = EQ
  compareWith Bitcoin _ = LT
  compareWith Ethereum Bitcoin = GT
  compareWith Ethereum Ethereum = EQ
  compareWith Ethereum Cardano = LT
  compareWith Cardano Cardano = EQ
  compareWith Cardano _ = GT

main3 = print $ compareWith Bitcoin Ethereum

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance (Eq a) => Eq (Box a) where
  Empty == Empty = True
  Has a == Has b = a == b
  _ == _ = False

main4 = print $ Has 5 == Has 5

-- HC10T5: ShowDetailed Type Class
data User = User { username :: String, userId :: Int }

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User name uid) = "User: " ++ name ++ " with ID " ++ show uid

main5 = print $ showDetailed (User "Fina" 101)

-- HC10T6: Mutual Recursion in Eq
instance Eq Blockchain where
  a == b = not (a /= b)
  a /= b = case compareWith a b of
    EQ -> False
    _ -> True

main6 = print $ Bitcoin == Ethereum

-- HC10T7: Convertible Type Class
class Convertible a b | a -> b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash = "Paid in Cash"
  convert Card = "Paid by Card"
  convert Cryptocurrency = "Paid with Crypto"

main7 = print $ convert Card

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq Int where
  compareEquality a b = a == b

main8 = print $ compareEquality 5 5

-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

main9 = print (minValue :: Int, maxValue :: Int)

-- HC10T10: Concatenatable
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable String where
  concatWith = (++)

main10 = print $ concatWith "Hello, " "World!"
