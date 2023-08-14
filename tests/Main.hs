module Main (main) where

import Control.Monad (foldM)

import GameOfClicks (minimumClicksIO, parse)
import Types (Test, Actual, Expected, Status)
import Tests (clicksTests, fileParseTests)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
main :: IO ()
main = do
  putStrLn "+++ Game of Clicks -- Tests."
  -- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  putStrLn " +++ Tests For Minimum Clicks For Viewable Channels Navigation."
  (_, failCount) <- foldM (runTest minimumClicksIO) (0, 0) clicksTests
  putStrLn " +++ Tests For Input File Parse."
  (_, failCount1) <- foldM (runTest parse)  (0, failCount) fileParseTests
  putStrLn ("+++ " ++ show failCount1 ++ " TEST FAILURES")

runTest :: (Show a, Show b)
        => (FilePath -> IO (Either a b))
        -> (Int, Int) -> Test
        -> IO (Int, Int)
runTest f (pCount, fCount) (filePath, expected) = do
  result <- f filePath
  let actual = case result of
        Right x   -> show x
        Left err  -> show err
  putStrLn $ formatTestResult filePath actual expected
  case () of
    _ | actual == expected -> return (pCount + 1, fCount)
      | otherwise          -> return (pCount, fCount + 1)

formatTestResult :: FilePath -> Actual -> Expected -> Status
formatTestResult testFile actual expected =
    let status = if actual == expected then "PASS" else "FAIL"
    in "  ++  " ++ "Test File:  '"  ++ testFile ++ "'\n"  ++
       "      " ++ "Actual:      "  ++ actual ++ "\n"     ++
       "      " ++ "Expected:    "  ++ expected ++ "\n"   ++
       "      " ++ "Status:      "  ++ status

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
