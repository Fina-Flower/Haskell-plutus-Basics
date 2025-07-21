--HC18T1: mapToLower Function with fmap
import Data.Char (toLower)
mapToLower :: String -> String
mapToLower = fmap toLower
main :: IO ()
main = do
  print (mapToLower "HeLLo WoRLd") -- "hello world"

--HC18T2: Functor Instance for Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)
instance Functor Tree where
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
main :: IO ()
main = do
  let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
  print tree

--HC18T3: incrementTreeValues Function
incrementTreeValues :: Tree Int -> Tree Int
incrementTreeValues = fmap (+1)
main :: IO ()
main = do
  let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
  print (incrementTreeValues tree)

--HC18T4: mapToBits Function
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')
main :: IO ()
main = do
  print (mapToBits [True, False, True, True]) -- "1011"

--HC18T5: Functor Instance for Either
instance Functor (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right y) = Right (f y)
main :: IO ()
main = do
  print (fmap (+1) (Right 4 :: Either String Int)) -- Right 5
  print (fmap (+1) (Left "Error"))                 -- Left "Error"

--HC18T6: applyToMaybe Function
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap
main :: IO ()
main = do
  print (applyToMaybe (*2) (Just 10)) -- Just 20
  print (applyToMaybe (*2) Nothing)   -- Nothing

--HC18T7: fmapTuple Function
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap
main :: IO ()
main = do
  print (fmapTuple (+1) ("score", 99)) -- ("score", 100)

--HC18T8: identityLawCheck Function
identityLawCheck :: (Eq f, Functor t) => t f -> Bool
identityLawCheck x = fmap id x == x
main :: IO ()
main = do
  print (identityLawCheck (Just "Hello")) -- True
  print (identityLawCheck [1, 2, 3])      -- True

--HC18T9: compositionLawCheck Function
compositionLawCheck :: (Eq c, Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x
main :: IO ()
main = do
  print (compositionLawCheck (*2) (+1) (Just 3)) -- True
  print (compositionLawCheck reverse tail ["hello", "world"]) -- True

--HC18T10: nestedFmap Function
nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap
main :: IO ()
main = do
  let x = [[Just 1, Nothing], [Just 2, Just 3]]
  print (nestedFmap (+1) x) -- [[Just 2,Nothing],[Just 3,Just 4]]
