--HC15T1: Handle Exceptions for File Reading and Velocity Calculation
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
main1 :: IO ()
main1 = do
  content <- catch (readFile "input.txt") handler
  putStrLn "Enter distance (meters):"
  dStr <- getLine
  putStrLn "Enter time (seconds):"
  tStr <- getLine
  case (readMaybe dStr, readMaybe tStr) of
    (Just d, Just t) -> if t /= 0 then print (d / t) else putStrLn "Time cannot be zero."
    _ -> putStrLn "Invalid input."
  where
    handler :: IOException -> IO String
    handler _ = putStrLn "File not found. Using default data." >> return ""

--HC15T2: Self-Driving AI Car System
respondToLight :: String -> String
respondToLight color =
  case color of
    "Red"    -> "Stop"
    "Yellow" -> "Slow down"
    "Green"  -> "Go"
    _        -> "Unknown color"
main2 :: IO ()
main2 = do
  putStrLn "Traffic light color?"
  color <- getLine
  putStrLn (respondToLight color)

--HC15T3: Custom Exception for Traffic Light Errors
import Control.Exception
data TrafficException = UnknownColorException String
  deriving Show
instance Exception TrafficException
checkTraffic :: String -> IO ()
checkTraffic "Red"    = putStrLn "Stop"
checkTraffic "Yellow" = putStrLn "Slow down"
checkTraffic "Green"  = putStrLn "Go"
checkTraffic color    = throwIO (UnknownColorException color)
main3 :: IO ()
main3 = checkTraffic "Blue"

--HC15T4: Exception Handler for Traffic Light
main4 :: IO ()
main4 = checkTraffic "Purple" `catch` handler
  where
    handler :: TrafficException -> IO ()
    handler (UnknownColorException color) = putStrLn $ "Error: Unknown color - " ++ color

--HC15T5: Safe Division Using Maybe
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)
main5 :: IO ()
main5 = print $ safeDiv 10 0

--HC15T6: Safe Input Parsing with readMaybe
import Text.Read (readMaybe)
main6 :: IO ()
main6 = do
  putStrLn "Enter a number:"
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just n  -> print (n * 2)
    Nothing -> putStrLn "Invalid number"

--HC15T7: Velocity Calculation with Optionals and Parsing Handling
main7 :: IO ()
main7 = do
  putStrLn "Enter distance (m):"
  dStr <- getLine
  putStrLn "Enter time (s):"
  tStr <- getLine
  case (readMaybe dStr, readMaybe tStr) of
    (Just d, Just t) ->
      case safeDiv d t of
        Just v  -> putStrLn $ "Velocity: " ++ show v ++ " m/s"
        Nothing -> putStrLn "Time cannot be zero"
    _ -> putStrLn "Invalid input"

--HC15T8: Division with Either for Detailed Errors
safeDivide :: Double -> Double -> Either String Double
safeDivide _ 0 = Left "Cannot divide by zero"
safeDivide x y = Right (x / y)
main8 :: IO ()
main8 = print $ safeDivide 10 0

--HC15T9: Try Function for File IO Exceptions
import System.IO
import Control.Exception (try, IOException)
main9 :: IO ()
main9 = do
  result <- try (readFile "file.txt") :: IO (Either IOException String)
  case result of
    Right content -> putStrLn content
    Left _        -> putStrLn "Failed to read the file"

--HC15T10: Hybrid Error Handling with Either and IO
getVelocity :: String -> String -> Either String Double
getVelocity ds ts = do
  d <- maybe (Left "Invalid distance") Right (readMaybe ds)
  t <- maybe (Left "Invalid time") Right (readMaybe ts)
  if t == 0
    then Left "Time cannot be zero"
    else Right (d / t)
main10 :: IO ()
main10 = do
  putStrLn "Enter distance:"
  dStr <- getLine
  putStrLn "Enter time:"
  tStr <- getLine
  case getVelocity dStr tStr of
    Right v -> putStrLn $ "Velocity: " ++ show v
    Left err -> putStrLn $ "Error: " ++ err
