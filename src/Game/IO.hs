{-# LANGUAGE ScopedTypeVariables #-}
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | IO API that takes a file input to compute the minimum clicks to navigate 
-- viewable TV channels listed in the file.
--
-- See ./docs/problem-statement.txt to learn about file input format.
-- See ./tests/good-file-inputs/normal.txt for a sample file input.
--
-- Author: Prem Muthedath
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Game.IO (minimumClicksIO, parse) where

import Text.Read (readMaybe)
import Data.List (lines, nub)
import Control.Monad.Reader (runReader)

import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

import Game.Types (Clicks, FileParseError (..), Input(..))
import Game.Core (minimumClicks)
import Game.Common (validate)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Minimum clicks for navigating viewable channels listed in the input file.
-- See ./docs/problem-statement.txt to learn about file input format.
-- See ./tests/good-file-inputs/normal.txt for a sample file input.
minimumClicksIO :: FilePath -> IO (Either FileParseError Clicks)
minimumClicksIO fp = do
    status <- isValidFilePath
    if status then do
        result <- parse fp
        case result of
          Right pInput -> return $ Right $ runReader minimumClicks pInput
          Left err     -> return $ Left $ FileParseError fp err
    else return $ Left $ FileParseError fp "Missing or non '.txt' file."
  where isValidFilePath :: IO Bool
        isValidFilePath = do
          exists <- doesFileExist fp
          let hasExtension = takeExtension fp == ".txt"
          return (exists && hasExtension)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Parse input data file to create an `Input` record.
-- See ./docs/problem-statement.txt to learn about file input format.
-- See ./tests/good-file-inputs/normal.txt for a sample file input.
parse :: FilePath -> IO (Either String Input)
parse fp = do
  lines' :: [String] <- Data.List.lines <$> readFile fp
  -- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
  return $ parse' $ sequence $ readMaybe <$> lines'
  where parse' :: Maybe [Int] -> Either String Input
        parse' (Just xs)
          | length xs < 4 = Left "Input file must have 4 or more lines."
          | any (< 0) xs  = Left "Input data must all be integers >= 0."
          | otherwise = do
            let lowest'     = head xs
                highest'    = head $ tail xs
                index1      = 2
                bcount      = xs !! index1
                blocked'    = nub $ take bcount $ drop (index1 + 1) xs
                index2      = index1 + 1 + bcount
                vcount      = if length xs > index2 then xs !! index2 else -1
                viewables'  = take vcount $ drop (index2 + 1) xs
            case () of
              _ | bcount /= length blocked' ->
                  Left "Specified blocked count /= # of blocked channels."
                | vcount < 0 ->
                  Left "No count of viewables found."
                | vcount /= length viewables' ->
                  Left "Specified viewables count /= # of viewable channels."
                | otherwise ->
                  validate $ Input lowest' highest' blocked' viewables'
        parse' Nothing = Left "Input file must contain only integers."

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
