{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (foldM)

import GameOfClicks (minimumClicksIO, FileParseError, Clicks)
import Types (Test, Actual, Expected, Status)
import Tests (tests)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
main :: IO ()
main = do
  putStrLn "+++ Game of Clicks -- Minimum Clicks For Viewable Channels Navigation."
  -- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  (_, failCount) <- foldM runTest (0, 0) tests
  putStrLn ("+++ " ++ show failCount ++ " TEST FAILURES")

runTest :: (Int, Int) -> Test -> IO (Int, Int)
runTest (pCount, fCount) (filePath, expected) = do
  result :: Either FileParseError Clicks <- minimumClicksIO filePath
  let actual = case result of
        Right x   -> show x
        Left err  -> show err
  putStrLn $ formatTestCase filePath actual expected
  case () of
    _ | actual == expected -> return (pCount + 1, fCount)
      | otherwise          -> return (pCount, fCount + 1)

formatTestCase :: FilePath -> Actual -> Expected -> Status
formatTestCase testFile actual expected =
    let status = if actual == expected then "PASS" else "FAIL"
    in "  ++  " ++ "Test File:  '"  ++ testFile ++ "'\n"  ++
       "      " ++ "Actual:      "  ++ actual ++ "\n"     ++
       "      " ++ "Expected:    "  ++ expected ++ "\n"   ++
       "      " ++ "Status:      "  ++ status

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
