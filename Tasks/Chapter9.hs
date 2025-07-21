-- HC9T1: Define a Parametric Type Synonym
type Entity a = (a, String)

main1 :: IO ()
main1 = print (("0x123ABC" :: String, "Wallet") :: Entity String)


-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving Show

main2 :: IO ()
main2 = do
  print (Has 5 :: Box Int)
  print (Empty :: Box Int)


-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (n + x)
addN _ Empty = Empty

main3 :: IO ()
main3 = do
  print (addN 10 (Has 5))    -- Has 15
  print (addN 10 Empty)      -- Empty


-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

main4 :: IO ()
main4 = do
  print (extract 0 (Has 7))  -- 7
  print (extract 0 Empty)    -- 0


-- HC9T5: Parametric Data Type with Record Syntax
data Shape a =
    Circle { radius :: Float, color :: a }
  | Rectangle { width :: Float, height :: Float, color :: a }
  deriving Show

main5 :: IO ()
main5 = do
  let c = Circle { radius = 5, color = "Red" }
  let r = Rectangle { width = 10, height = 4, color = "Blue" }
  print c
  print r


-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content :: String
  , likes :: Int
  , comments :: [Tweet]
  } deriving Show

main6 :: IO ()
main6 = do
  let comment1 = Tweet "Nice!" 3 []
  let comment2 = Tweet "I agree!" 5 []
  let mainTweet = Tweet "Hello Haskell!" 10 [comment1, comment2]
  print mainTweet


-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

main7 :: IO ()
main7 = do
  let c1 = Tweet "Nice!" 3 []
  let c2 = Tweet "Great!" 2 []
  let t = Tweet "Hello" 5 [c1, c2]
  print (engagement t)  -- 5 + 3 + 2 = 10


-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving Show

main8 :: IO ()
main8 = do
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1


-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest) = x == y || elemSeq x rest

main9 :: IO ()
main9 = do
  let s = Node 1 (Node 2 (Node 3 End))
  print (elemSeq 2 s) -- True
  print (elemSeq 5 s) -- False


-- HC9T10: Binary Search Tree Data Type
data BST a = Nil | NodeBST a (BST a) (BST a) deriving Show

main10 :: IO ()
main10 = do
  let tree = NodeBST 10
               (NodeBST 5 Nil Nil)
               (NodeBST 15 (NodeBST 12 Nil Nil) Nil)
  print tree
