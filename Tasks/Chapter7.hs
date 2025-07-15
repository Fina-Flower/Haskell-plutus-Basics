-- HC7T1: Eq instance for Color
data Color = Red | Green | Blue deriving (Show)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

main1 :: IO ()
main1 = do
  print (Red == Red)     -- True
  print (Red == Blue)    -- False


-- HC7T2: Ord instance for Color
instance Ord Color where
  compare Red Green = LT
  compare Red Blue  = LT
  compare Green Blue = LT
  compare Green Red = GT
  compare Blue Red = GT
  compare Blue Green = GT
  compare c1 c2
    | c1 == c2 = EQ
    | otherwise = error "Invalid comparison"

main2 :: IO ()
main2 = do
  print (Red < Green)   -- True
  print (Blue > Red)    -- True
  print (Red == Red)    -- True


-- HC7T3: Function with Eq and Ord constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

main3 :: IO ()
main3 = do
  print (compareValues 3 5)  -- 5
  print (compareValues 'z' 'a') -- 'z'


-- HC7T4: Show and Read for Shape
data Shape = Circle Double | Rectangle Double Double deriving (Eq)

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input =
    case words input of
      ("Circle":r:_) ->
        [(Circle (read r), "")]
      ("Rectangle":w:h:_) ->
        [(Rectangle (read w) (read h), "")]
      _ -> []

main4 :: IO ()
main4 = do
  print (show (Circle 5))
  print (read "Rectangle 3 4" :: Shape)


-- HC7T5: Function with Num constraint
squareArea :: Num a => a -> a
squareArea s = s * s

main5 :: IO ()
main5 = do
  print (squareArea 4)     -- 16
  print (squareArea 3.5)   -- 12.25


-- HC7T6: Integral and Floating
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r

main6 :: IO ()
main6 = do
  print (circleCircumference 7)    -- ~43.98
  print (circleCircumference 7.5)  -- ~47.12


-- HC7T7: Bounded and Enum
instance Enum Color where
  toEnum 0 = Red
  toEnum 1 = Green
  toEnum 2 = Blue
  toEnum _ = error "Invalid enum for Color"

  fromEnum Red   = 0
  fromEnum Green = 1
  fromEnum Blue  = 2

instance Bounded Color where
  minBound = Red
  maxBound = Blue

nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise     = succ c

main7 :: IO ()
main7 = do
  print (nextColor Red)    -- Green
  print (nextColor Green)  -- Blue
  print (nextColor Blue)   -- Red


-- HC7T8: Parse a Shape safely
parseShape :: String -> Maybe Shape
parseShape s =
  case reads s of
    [(shape, "")] -> Just shape
    _             -> Nothing

main8 :: IO ()
main8 = do
  print (parseShape "Circle 5")            -- Just (Circle 5.0)
  print (parseShape "Rectangle 3 4")       -- Just (Rectangle 3.0 4.0)
  print (parseShape "Triangle 3 4 5")      -- Nothing


-- HC7T9: Describable type class
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true."
  describe False = "This is false."

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle of width " ++ show w ++ " and height " ++ show h

main9 :: IO ()
main9 = do
  print (describe True)
  print (describe (Circle 7))


-- HC7T10: describeAndCompare
describeAndCompare :: (Ord a, Describable a) => a -> a -> String
describeAndCompare a b = describe (if a > b then a else b)

main10 :: IO ()
main10 = do
  print (describeAndCompare True False)          -- "This is true."
  print (describeAndCompare (Rectangle 4 5) (Circle 6)) -- Depends on derived ordering
