--HC17T1: Severity Data Type and Semigroup Instance
data Severity = Low | Medium | High | Critical
  deriving (Eq, Show, Enum, Bounded)
instance Semigroup Severity where
  a <> b = max a b
main :: IO ()
main = do
  print (Medium <> High)      -- High
  print (Low <> Medium)       -- Medium
  print (Critical <> Low)     -- Critical

--HC17T2: Min and Max Newtypes with Semigroup
newtype Min a = Min a deriving (Show)
newtype Max a = Max a deriving (Show)
instance (Ord a) => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)
instance (Ord a) => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)
main :: IO ()
main = do
  print (Min 3 <> Min 5) -- Min 3
  print (Max 3 <> Max 5) -- Max 5

--HC17T3: Monoid Instance for Severity
instance Monoid Severity where
  mempty = Low
main :: IO ()
main = do
  print (mempty <> High)     -- High
  print (Critical <> mempty) -- Critical

--HC17T4: Monoid Instance for Sum Newtype
newtype Sum a = Sum a deriving (Show)
instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
main :: IO ()
main = do
  print (Sum 3 <> Sum 4)   -- Sum 7
  print (mempty <> Sum 5)  -- Sum 5

--HC17T5: combineLists Function
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)
main :: IO ()
main = do
  print (combineLists [1,2] [3,4]) -- [1,2,3,4]

--HC17T6: maxSeverity Function
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat
main :: IO ()
main = do
  print (maxSeverity [Low, Medium, Critical, High]) -- Critical

--HC17T7: multiplyProducts Function
newtype Product a = Product a deriving (Show)
instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)
instance Num a => Monoid (Product a) where
  mempty = Product 1
multiplyProducts :: Num a => [Product a] -> Product a
multiplyProducts = mconcat
main :: IO ()
main = do
  print (multiplyProducts [Product 2, Product 3, Product 4]) -- Product 24

--HC17T8: foldWithSemigroup Function
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)
main :: IO ()
main = do
  print (foldWithSemigroup [[1], [2,3], [4]]) -- [1,2,3,4]
  print (foldWithSemigroup [High, Medium, Critical]) -- Critical
  
  --HC17T9: Config Data Type and Semigroup Instance
data Config = Config
  { loggingLevel :: Severity
  , timeout      :: Int
  , retries      :: Int
  } deriving (Show)
instance Semigroup Config where
  c1 <> c2 = Config
    { loggingLevel = loggingLevel c1 <> loggingLevel c2
    , timeout      = min (timeout c1) (timeout c2)
    , retries      = max (retries c1) (retries c2)
    }
main :: IO ()
main = do
  let c1 = Config Medium 60 2
  let c2 = Config High 30 5
  print (c1 <> c2)

--HC17T10: Monoid Instance for Config
instance Monoid Config where
  mempty = Config Low maxBound 0
main :: IO ()
main = do
  let defaultCfg = mempty
  let customCfg = Config High 100 3
  print (defaultCfg <> customCfg)
