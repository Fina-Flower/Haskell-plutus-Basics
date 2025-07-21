--HC20T1: safeDivide with Maybe
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)
main1 :: IO ()
main1 = print $ safeDivide 10 2 -- Just 5

--HC20T2: sequenceMaybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (Just x:xs) = (x:) <$> sequenceMaybe xs
sequenceMaybe (Nothing:_) = Nothing
main2 :: IO ()
main2 = print $ sequenceMaybe [Just 1, Just 2, Just 3] -- Just [1,2,3]

--HC20T3: Writer Monad Logging Calculator
import Control.Monad.Writer
type Calc = Writer [String] Int
add :: Int -> Int -> Calc
add x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])
main3 :: IO ()
main3 = print $ runWriter (add 4 5)

--HC20T4: countChars with State Monad
import Control.Monad.State
countChars :: Char -> String -> Int
countChars c s = execState (mapM_ check s) 0
  where check x = when (x == c) $ modify (+1)
main4 :: IO ()
main4 = print $ countChars 'a' "banana"

--HC20T5: Reader Monad Greeting
import Control.Monad.Reader
type Config = String
greet :: Reader Config String
greet = do
  name <- ask
  return $ "Hello, " ++ name ++ "!"
main5 :: IO ()
main5 = print $ runReader greet "Alice"

--HC20T6: doubleMonad
doubleMonad :: Maybe [a] -> [a]
doubleMonad (Just xs) = xs
doubleMonad Nothing = []
main6 :: IO ()
main6 = print $ doubleMonad (Just [1,2,3])

--HC20T7: findFirst with Either
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "Not found"
findFirst f (x:xs) = if f x then Right x else findFirst f xs
main7 :: IO ()
main7 = print $ findFirst even [1,3,4,5]

--HC20T8: Simple Expression Parser (mock)
type Parser a = String -> Maybe (a, String)
char :: Char -> Parser Char
char c (x:xs) | c == x = Just (c, xs)
char _ _ = Nothing
main8 :: IO ()
main8 = print $ char 'a' "abc"

--HC20T9: replicateMonad with Identity
import Data.Functor.Identity
replicateMonad :: Int -> Identity a -> Identity [a]
replicateMonad n m = Identity (replicate n (runIdentity m))
main9 :: IO ()
main9 = print $ runIdentity (replicateMonad 3 (Identity 5))

--HC20T10: Nested StateT and MaybeT
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
type Nested = MaybeT (State Int)
incrementIfOdd :: Nested ()
incrementIfOdd = do
  x <- lift get
  if odd x then lift (put (x + 1)) else MaybeT (return Nothing)
main10 :: IO ()
main10 = print $ runState (runMaybeT incrementIfOdd) 3

--HC20T11: randomWalk with State
import System.Random
type Position = (Int, Int)
type Walk = State StdGen Position
randomWalk :: Int -> Walk
randomWalk 0 = return (0, 0)
randomWalk n = do
  (x, y) <- randomWalk (n - 1)
  dir <- state random
  return $ case (dir `mod` 4) of
    0 -> (x+1, y)
    1 -> (x-1, y)
    2 -> (x, y+1)
    _ -> (x, y-1)
main11 :: IO ()
main11 = do
  gen <- newStdGen
  print $ evalState (randomWalk 10) gen

--HC20T12: File Reading with IO Monad
main12 :: IO ()
main12 = do
  contents <- readFile "example.txt"
  mapM_ putStrLn (lines contents)

--HC20T13: Fibonacci Memoization
import qualified Data.Map as Map
type FibState = Map.Map Int Int
fibonacciMemo :: Int -> State FibState Int
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
  m <- get
  case Map.lookup n m of
    Just val -> return val
    Nothing -> do
      a <- fibonacciMemo (n-1)
      b <- fibonacciMemo (n-2)
      let val = a + b
      modify (Map.insert n val)
      return val
main13 :: IO ()
main13 = print $ evalState (fibonacciMemo 10) Map.empty

--HC20T14: mapMFilter
mapMFilter :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMFilter f [] = return []
mapMFilter f (x:xs) = do
  y <- f x
  ys <- mapMFilter f xs
  return $ maybe ys (:ys) y
main14 :: IO ()
main14 = print =<< mapMFilter (\x -> return (if even x then Just x else Nothing)) [1..5]

--HC20T15: treeSum with Custom Monad
data Tree a = Leaf a | Node (Tree a) (Tree a)
treeSum :: Tree Int -> Int
treeSum (Leaf x) = x
treeSum (Node l r) = treeSum l + treeSum r
main15 :: IO ()
main15 = print $ treeSum (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))

--HC20T16: retryIO
retryIO :: Int -> IO (Maybe Int) -> IO (Maybe Int)
retryIO 0 _ = return Nothing
retryIO n action = do
  result <- action
  case result of
    Just x -> return (Just x)
    Nothing -> retryIO (n - 1) action
main16 :: IO ()
main16 = retryIO 3 (return Nothing) >>= print

--HC20T17: validatePassword
validatePassword :: String -> Either String String
validatePassword pwd
  | length pwd < 8 = Left "Too short"
  | not (any (`elem` ['A'..'Z']) pwd) = Left "No uppercase"
  | otherwise = Right "Valid password"
main17 :: IO ()
main17 = print $ validatePassword "secret"

--HC20T18: MaybeT User Input
askName :: MaybeT IO String
askName = MaybeT $ do
  putStr "Enter name: "
  name <- getLine
  return $ if null name then Nothing else Just name
main18 :: IO ()
main18 = do
  result <- runMaybeT askName
  case result of
    Just name -> putStrLn ("Hello " ++ name)
    Nothing -> putStrLn "No name given."

--HC20T19: Writer Monad Logging System
logFunction :: Int -> Writer [String] Int
logFunction x = writer (x * 2, ["Doubled " ++ show x])
main19 :: IO ()
main19 = print $ runWriter (logFunction 3)

--HC20T20: batchProcessing with Bind
batchProcessing :: [IO ()] -> IO ()
batchProcessing = foldr (>>) (return ())
main20 :: IO ()
main20 = batchProcessing [print 1, print 2, print 3]
