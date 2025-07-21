-- HC11T1: WeAccept instance for Box
class WeAccept a where
  accept :: a -> Bool

instance WeAccept (Box a) where
  accept Empty = False
  accept (Has _) = True

main11 = print $ accept (Has 5)

-- HC11T2: Fancy Function
fancyFunction :: Show a => a -> String
fancyFunction x = "You chose: " ++ show x

main12 = print $ fancyFunction Cardano

-- HC11T3: Container Type Class for Box
class Container c where
  isEmpty :: c a -> Bool
  contains :: Eq a => a -> c a -> Bool
  replace :: a -> c a -> c a

instance Container Box where
  isEmpty Empty = True
  isEmpty _ = False
  contains _ Empty = False
  contains x (Has y) = x == y
  replace x _ = Has x

main13 = do
  print $ isEmpty (Has 1)
  print $ contains 5 (Has 5)
  print $ replace 9 (Has 1)

-- HC11T4: Container for Present
data Present a = NoGift | Gift a deriving (Show)

instance Container Present where
  isEmpty NoGift = True
  isEmpty _ = False
  contains _ NoGift = False
  contains x (Gift y) = x == y
  replace x _ = Gift x

main14 = do
  print $ isEmpty NoGift
  print $ contains "Ball" (Gift "Ball")

-- HC11T5: guessWhat'sInside
guessWhat'sInside :: (Container c, Eq a, Show a) => a -> c a -> String
guessWhat'sInside x box =
  if contains x box then "It's " ++ show x ++ "!" else "Not here."

main15 = print $ guessWhat'sInside 42 (Has 42)

-- HC11T6: AdvancedEq for Blockchain
instance AdvancedEq Blockchain where
  compareEquality a b = a == b

main16 = print $ compareEquality Ethereum Ethereum

-- HC11T7: Ord Instance for Box
instance (Ord a) => Ord (Box a) where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Has a) (Has b) = compare a b

main17 = print $ Has 2 > Has 1

-- HC11T8: Deriving Eq and Ord
data PayMethod = PMCash | PMCard | PMCrypto deriving (Eq, Ord, Show)

main18 = print $ PMCash < PMCrypto

-- HC11T9: Length with Units
data Length = M Float | Km Float deriving (Show, Eq)

instance Ord Length where
  compare (M m1) (M m2) = compare m1 m2
  compare (Km k1) (Km k2) = compare k1 k2
  compare (M m) (Km k) = compare m (k * 1000)
  compare (Km k) (M m) = compare (k * 1000) m

main19 = print $ Km 1 > M 500

-- HC11T10: sortContainers
sortContainers :: Ord a => [a] -> [a]
sortContainers = sort

main20 = print $ sortContainers [PMCrypto, PMCash, PMCard]
