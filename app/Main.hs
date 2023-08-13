{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import System.Environment (getArgs)

import GameOfClicks (minimumClicksIO, FileParseError, Clicks)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
