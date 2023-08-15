-- | Test module to runs tests.
-- Author: Prem Muthedath
--
-- You can, when you are in the `game-of-clicks/` directory, run the tests in 
-- the terminal as follows:
--    `cabal v2-run -f testing :game-of-clicks-tests`
-- See ./tests/good-file-inputs/normal.txt for a sample test file input.
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Main (main) where

import Control.Monad (foldM)

import GameOfClicks (minimumClicksIO, parse, minimumClicksFor, Input (..))
import Types (TestInput, FileTest, InputTest, Actual, Expected, Status)
import Tests (clicksTests, fileParseTests, inputTests)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | `main` runs all tests and outputs test results and the final fail count.
main :: IO ()
main = do
  putStrLn "+++ Game of Clicks -- Tests."
  -- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  putStrLn " +++ Tests For Input File Parse."
  (_, failCount) <- foldM (runFileTest parse) (0, 0) fileParseTests
  putStrLn " +++ File Tests For Minimum Clicks For Viewable TV Channels Navigation."
  (_, failCount1) <- foldM (runFileTest minimumClicksIO) (0, failCount) clicksTests
  putStrLn " +++ Non-File Tests for Minimum Clicks for Viewable TV Channels Navigation."
  (_, failCount2) <- foldM runInputTest (0, failCount1) inputTests
  putStrLn ("+++ " ++ show failCount2 ++ " TEST FAILURES")

-- | Run a `FileTest`, print the result, and report back the pass-fail count.
runFileTest :: (Show a, Show b)
        => (FilePath -> IO (Either a b))
        -> (Int, Int)
        -> FileTest
        -> IO (Int, Int)
runFileTest f (pCount, fCount) (filePath, expected) = do
  result <- f filePath
  let actual = case result of
        Right x   -> show x
        Left err  -> show err
  putStrLn $ formatTestResult filePath actual expected
  case () of
    _ | actual == expected -> return (pCount + 1, fCount)
      | otherwise          -> return (pCount, fCount + 1)

-- | Run an `InputTest`, print the result, and report back the pass-fail count.
-- We run just 1 canned test here, because this code being tested is merely a 
-- wrapper around the code already tested in file tests.
runInputTest :: (Int, Int) -> InputTest -> IO (Int, Int)
runInputTest (pCount, fCount) (input, expected) = do
  let actual = case minimumClicksFor low high blkd vwbls of
        Right clks  -> show clks
        Left err    -> err
  putStrLn $ formatTestResult (show input) actual expected
  return $ if actual == expected then (pCount, fCount) else (pCount, fCount + 1)
  where low   = lowest input
        high  = highest input
        blkd  = blocked input
        vwbls = viewables input

-- | Format a `Test` result.
formatTestResult :: TestInput -> Actual -> Expected -> Status
formatTestResult input actual expected =
    let status = if actual == expected then "PASS" else "FAIL"
    in "  ++  " ++ "Test Input:  '" ++ input ++ "'\n"     ++
       "      " ++ "Actual:      "  ++ actual ++ "\n"     ++
       "      " ++ "Expected:    "  ++ expected ++ "\n"   ++
       "      " ++ "Status:      "  ++ status

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
