--HC14T1: Initialize a Cabal Project
You’d run this in the terminal:
cabal init --non-interactive --minimal --package-name=hc14project --exe
Then, edit app/Main.hs to:
main :: IO ()
main = putStrLn "Hello, Cabal!"

--HC14T2: Add Dependency and Print Random Number
Add to your .cabal file under build-depends:
random >= 1.2
In app/Main.hs:
import System.Random (randomRIO)
main2 :: IO ()
main2 = do
  num <- randomRIO (1, 100)
  putStrLn $ "Random number: " ++ show num

--HC14T3: NumericUnderscores Extension
{-# LANGUAGE NumericUnderscores #-}
main3 :: IO ()
main3 = do
  let bigNumber = 1_000_000
      another = 123_456_789
  print bigNumber
  print another

--HC14T4: TypeApplications Extension
{-# LANGUAGE TypeApplications #-}
main4 :: IO ()
main4 = do
  let num = read @Int "42"
  print num

--HC14T5: Custom Data Type and Pattern Matching with @
data Result a = Success a | Failure String deriving Show
handleResult :: Result Int -> String
handleResult res@(Success val) = "Success with value: " ++ show val ++ " | full: " ++ show res
handleResult res@(Failure msg) = "Failure with message: " ++ msg ++ " | full: " ++ show res
main5 :: IO ()
main5 = do
  print $ handleResult (Success 10)
  print $ handleResult (Failure "Oops")

--HC14T6: Project Structure: src and app
Update your Cabal file:
  exposed-modules:     Lib
  hs-source-dirs:      src
  build-depends:       base >=4.14 && <5
  default-language:    Haskell2010
executable hc14project
  main-is:             Main.hs
  hs-source-dirs:      app
  build-depends:       base, hc14project
  default-language:    Haskell2010
src/Lib.hs:
module Lib (sayHello) where
sayHello :: IO ()
sayHello = putStrLn "Hello from Library!"
app/Main.hs:
import Lib (sayHello)
main6 :: IO ()
main6 = sayHello

--HC14T7: Library Component in Cabal
Already done above in Task 6 — your .cabal file now supports both library and executable components.

--HC14T8: Character Frequency Function
import Data.List (group, sort)
counts :: String -> [(Char, Int)]
counts str = map (\g -> (head g, length g)) . group . sort $ str
main8 :: IO ()
main8 = print $ counts "haskell rocks"

--HC14T9: PartialTypeSignatures Extension
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
countChars :: String -> [(Char, Int)]
countChars = counts -- use from task 8
main9 :: IO ()
main9 = print (countChars "hello world")

--HC14T10: Cabal Test Suite
Add to .cabal file:
test-suite hc14project-test
  type:               exitcode-stdio-1.0
  hs-source-dirs:     test
  main-is:            Spec.hs
  build-depends:      base, hc14project
  default-language:   Haskell2010
Create test/Spec.hs:
import Lib (sayHello)
import Data.List (sort)
import System.Exit (exitFailure, exitSuccess)
import qualified Main8 as C
testCounts :: Bool
testCounts = sort (C.counts "aab") == [('a',2),('b',1)]
main :: IO ()
main = do
  if testCounts
    then putStrLn "All tests passed!" >> exitSuccess
    else putStrLn "Tests failed!" >> exitFailure
