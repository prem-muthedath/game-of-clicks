{-# LANGUAGE ScopedTypeVariables #-}
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | `game-clicks-app` module.
-- Author: Prem Muthedath.
--
-- In GHCi, when you are in the `game-of-clicks/` directory, you can load and 
-- run the app as this example below shows:
--  1. `cabal v2-repl -f testing :game-of-clicks-app`
--  2. `:set args "./tests/good-file-inputs/normal.txt"`
--  3. `main`
-- Or, using `cabal v2-run`, you can, when you are in the `game-of-clicks/` 
-- directory, load and run the app in one of the two following ways, as this 
-- example below shows:
--  1. `cabal v2-run -f testing :game-of-clicks-app -- "tests/good-file-inputs/normal.txt"`
--  2. `cabal v2-run -f testing :game-of-clicks-app "tests/good-file-inputs/normal.txt"`
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Main(main) where

import System.Environment (getArgs)

import GameOfClicks (minimumClicksIO, FileParseError, Clicks)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | `main` function when supplied an argument, a file path, computes the 
-- minimum clicks for navigating viewable channles specified in the file.
--
-- See ./docs/problem-statement.txt to learn about file input format.
-- See ./tests/good-file-inputs/normal.txt for a sample file input.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      result :: Either FileParseError Clicks <- minimumClicksIO arg
      case result of
        Left err      -> print err
        Right clicks  -> putStrLn $
            "Minimum Cicks For Viewable Channels Navigation: " ++ show clicks
    _     -> putStrLn "Please provide a single commandline argument\
                       \ which should be the file path of the input file."

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
