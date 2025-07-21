--HC13T1: List Files in Directory
import System.Directory (listDirectory)
main1 :: IO ()
main1 = do
  files <- listDirectory "."
  mapM_ putStrLn files

--HC13T2: Filter Files by Substring
import System.Directory (listDirectory)
import Data.List (isInfixOf)
filterFilesBySubstring :: String -> IO [FilePath]
filterFilesBySubstring substring = do
  files <- listDirectory "."
  return $ filter (isInfixOf substring) files
main2 :: IO ()
main2 = do
  result <- filterFilesBySubstring "hs"
  mapM_ putStrLn result

--HC13T3: Sort and Return Filtered Files
import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)
sortedFilteredFiles :: String -> IO [FilePath]
sortedFilteredFiles substring = do
  files <- listDirectory "."
  return $ sort $ filter (isInfixOf substring) files
main3 :: IO ()
main3 = do
  result <- sortedFilteredFiles "hs"
  mapM_ putStrLn result

--HC13T4: SumNonEmpty Module
File: SumNonEmpty.hs
module SumNonEmpty (sumNonEmpty) where
sumNonEmpty :: [Int] -> Int
sumNonEmpty [] = error "List cannot be empty"
sumNonEmpty xs = sum xs

--HC13T5: Restrict Module Export List
Still in SumNonEmpty.hs, now hiding helper functions (refactored if there were any):
module SumNonEmpty (sumNonEmpty) where
sumNonEmpty :: [Int] -> Int
sumNonEmpty = safeSum
  where
    safeSum [] = error "List cannot be empty"
    safeSum xs = sum xs

--HC13T6: File Names to Map
import qualified Data.Map as Map
import System.Directory (listDirectory)
import Data.List (isInfixOf)
fileNamesToMap :: String -> IO (Map.Map Int FilePath)
fileNamesToMap substring = do
  files <- listDirectory "."
  let filtered = filter (isInfixOf substring) files
  return $ Map.fromList (zip [1..] filtered)
main6 :: IO ()
main6 = do
  fileMap <- fileNamesToMap "hs"
  print fileMap

--HC13T7: Use Custom Module in Main
import SumNonEmpty (sumNonEmpty)
main7 :: IO ()
main7 = print $ sumNonEmpty [1, 2, 3, 4]

--HC13T8: Qualified Imports for Name Conflicts
import qualified Data.List as L
import qualified Data.Map as M
main8 :: IO ()
main8 = do
  let listSorted = L.sort [3, 1, 2]
  let mapExample = M.fromList [(1, "a"), (2, "b")]
  print listSorted
  print (M.lookup 1 mapExample)

--HC13T9: Renaming Module Namespace
import qualified Data.List as DL
import qualified Data.Map.Strict as DM
main9 :: IO ()
main9 = do
  let sorted = DL.sort ["z", "a", "m"]
  let dict = DM.fromList [(1, "Hello"), (2, "World")]
  print sorted
  print (DM.lookup 2 dict)

--HC13T10: Multi-Module Main Function
import System.Directory (listDirectory)
import qualified Data.List as DL
main10 :: IO ()
main10 = do
  files <- listDirectory "."
  let filtered = DL.filter (DL.isInfixOf "hs") files
  let sorted = DL.sort filtered
  putStrLn "Sorted Haskell-related files:"
  mapM_ putStrLn sorted
