--HC19T1: Applicative Instance for Pair
data Pair a = Pair a a deriving Show
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)
main :: IO ()
main = print (Pair (+1) (*2) <*> Pair 3 4) -- Pair 4 8

--HC19T2: addThreeApplicative
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)
main :: IO ()
main = print (addThreeApplicative (Just 1) (Just 2) (Just 3)) -- Just 6

--HC19T3: safeProduct
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA
main :: IO ()
main = do
  print (safeProduct [Just 2, Just 3, Just 4])   -- Just 24
  print (safeProduct [Just 2, Nothing, Just 4])  -- Nothing

--HC19T4: liftAndMultiply
liftAndMultiply :: Int -> Int -> Int
liftAndMultiply = (*) 
main :: IO ()
main = print (liftA2 liftAndMultiply (Just 4) (Just 3)) -- Just 12

--HC19T5: applyEffects
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (a, b) = (+) <$> a <*> b
main :: IO ()
main = do
  let x = return 5
  let y = return 10
  result <- applyEffects (x, y)
  print result

--HC19T6: repeatEffect
import Control.Monad (forever)
repeatEffect :: IO ()
repeatEffect = forever $ putStrLn "Repeating..."
main :: IO ()
main = return () -- Uncomment below to run forever

--HC19T7: conditionalPrint
import Control.Monad (when)
conditionalPrint :: Bool -> IO ()
conditionalPrint condition = when condition (putStrLn "Condition met!")
main :: IO ()
main = do
  conditionalPrint True   -- prints
  conditionalPrint False  -- doesn't print

--HC19T8: discardSecond
discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)
main :: IO ()
main = do
  result <- discardSecond (putStrLn "First") (putStrLn "Second")
  return result

--HC19T9: pureAndApply
pureAndApply :: Int
pureAndApply = (pure (+1) <*> pure 5)
main :: IO ()
main = print pureAndApply -- 6

--HC19T10: combineResults
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)
main :: IO ()
main = do
  print (combineResults (Right 3) (Right 4)) -- Right 7
  print (combineResults (Left "Error") (Right 4)) -- Left "Error"
