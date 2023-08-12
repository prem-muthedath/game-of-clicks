{-# LANGUAGE ScopedTypeVariables #-}
--
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Game Of Clicks (R->Hard)
-- thoughtworks interview problem
-- https://prepinsta.com/thoughtworks-coding-questions/
-- see also ./docs/problem-statement.txt
--
-- Haskell Solution.
-- author: Prem Muthedath, AUG 2023.
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Main where

import Control.Monad.Reader (Reader, runReader, ask, asks, forM, foldM)
import Data.List (lines, nub)
import Text.Read (readMaybe)

import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- First, define some type synonyms.
type Clicks = Int; type Channel = Int

-- `Input` record to capture/store all input we need for this problem.
data Input = Input { lowest, highest :: Channel
                   , blocked, viewables :: [Channel]
                   } deriving (Show, Eq)

-- Type synonym for Reader monad usage.
type ClicksReader a = Reader Input a

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Covenience function that enables you to see output of functions in this 
-- module that operate in the Reader monad. Because we do not have a `Show` 
-- instance for `ClicksReader a` defined (it is not easy to do!), it becomes 
-- difficult to test/debug functions in this module in GHCi. But with this 
-- function, you can quite easily do so.
--
-- Examples:
-- runClicksReader (upDownClicks 1 20) (Input {lowest = 1, highest = 20, blocked 
-- = [18,19], viewables = [15,14,17,11,17]})
--
-- runClicksReader viewableHigh (Input {lowest = 1, highest = 20, blocked = 
-- [18,19], viewables = [15,14,17,11,17]})
runClicksReader :: Show a => ClicksReader a -> Input -> a
runClicksReader = runReader

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Mimimum clicks to move from one channel to another using up-down keys.
upDownClicks :: Channel -> Channel -> ClicksReader Clicks
upDownClicks from to = do
  input <- ask
  if all (`notElem` blocked input) [from, to]
    then do up    <- upNavigationClicks
            min up <$> downNavigationClicks
    else error $ show [from, to] ++ " has blocked channels"
  where upNavigationClicks :: ClicksReader Clicks
        upNavigationClicks = do
          vHigh <- viewableHigh
          vLow  <- viewableLow
          case () of
            _ | from == vHigh -> totalClicks [vLow .. to]
              | from < to -> totalClicks [from + 1 .. to]
              | from > to -> do
                  clks1 <- totalClicks [from + 1 .. vHigh]
                  clks2 <- totalClicks [vLow .. to]
                  return $ clks1 + clks2
              | otherwise -> return 0
        downNavigationClicks :: ClicksReader Clicks
        downNavigationClicks = do
          vHigh <- viewableHigh
          vLow  <- viewableLow
          case () of
            _ | from == vLow -> totalClicks [vHigh, vHigh - 1 .. to]
              | from < to -> do
                  clks1 <- totalClicks [from-1, from-2 .. vLow]
                  clks2 <- totalClicks [vHigh, vHigh-1 .. to]
                  return $ clks1 + clks2
              | from > to -> totalClicks [from-1, from-2 .. to]
              | otherwise -> return 0
        totalClicks :: [Channel] -> ClicksReader Clicks
        totalClicks chs = do
          input <- ask
          let blkd = blocked input
          return $ length $ filter (`notElem` blkd) chs

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Highest unblocked (i.e., viewable) channel.
viewableHigh :: ClicksReader Channel
viewableHigh = do
  input <- ask
  let high = highest input
      blkd = blocked input
  case () of
    _ | high `notElem` blkd -> return high
      | otherwise -> do
        rChannels <- reverse <$> allChannels
        return $ head $ dropWhile (`elem` blkd) rChannels

-- | Lowest unblocked (i.e., viewable) channel.
viewableLow :: ClicksReader Channel
viewableLow = do
  input <- ask
  let low   = lowest input
      blkd  = blocked input
  case () of
    _ | low `notElem` blkd -> return low
      | otherwise -> head . dropWhile (`elem` blkd) <$> allChannels

-- | All channels (blocked + viewable) in ascending order of channel numbers.
allChannels :: ClicksReader [Channel]
allChannels = do
  input <- ask
  return [lowest input .. highest input]

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | # of key-press clicks to directly access a channel.
keyPressClicks :: Channel -> Clicks
keyPressClicks = length . show

-- | # of clicks to access the previous channel using the `Last Viewed` button.
lastViewedClicks :: Channel -> Channel -> Clicks
lastViewedClicks prev to  | prev == to = 1
                          | otherwise = maxBound

-- | # of clicks to directly access the 1st channel in the viewing sequence.
clicksToGetStarted :: ClicksReader Clicks
clicksToGetStarted = asks (keyPressClicks . head . viewables)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Minimum # of clicks to navigate all the channels in the viewing sequence.
minimumClicks :: ClicksReader Clicks
minimumClicks = do
  input <- ask
  case viewables input of
    []  -> return 0
    [_] -> clicksToGetStarted
    _   -> sum <$> allMinClicks
  where allMinClicks :: ClicksReader [Clicks]
        allMinClicks = do
          first <- clicksToGetStarted
          input <- ask
          let vChannels = viewables input
              viewableTriples = zip3 (maxBound:vChannels) vChannels (tail vChannels)
          rest <- forM viewableTriples $
            \(prev, from, to) -> do
                let lastViewed :: Clicks = lastViewedClicks prev to
                    keyPressed :: Clicks = keyPressClicks to
                upDown :: Clicks <- upDownClicks from to
                return $ minimum [lastViewed, upDown, keyPressed]
          return (first:rest)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Parse input data to create an `Input` record.
-- See ./docs/problem-statement.txt to learn about input format.
-- see ./tests/good-inputs/normal.txt for a sample input.
parse :: FilePath -> IO (Either String Input)
parse fp = do
  lines' :: [String] <- Data.List.lines <$> readFile fp
  -- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
  return $ parse' $ sequence $ readMaybe <$> lines'
  where parse' :: Maybe [Int] -> Either String Input
        parse' (Just xs)
          | length xs < 4 = Left "Input file must have 4 or more lines."
          | any (< 0) xs  = Left "Input data must all be integers >= 0."
          | otherwise =
            let lowest'     = head xs
                highest'    = head $ tail xs
                bcount      = xs !! 2
                blocked'    = nub $ take bcount $ drop (2 + 1) xs
                vcount      = let index = (2 + 1 + bcount)
                              in if length xs > index then xs !! index else -1
                viewables'  = take vcount $ drop (2 + 1 + bcount + 1) xs
            in if bcount /= length blocked'
                  then Left "Specified blocked count /= # of blocked channels."
               else if vcount < 0
                  then Left "No count of viewables found."
               else if vcount /= length viewables'
                  then Left "Specified viewables count /= # of viewable channels."
               else validate Input { lowest = lowest'
                                   , highest = highest'
                                   , blocked = blocked'
                                   , viewables = viewables'
                                   }
        parse' Nothing = Left "Input file must contain only integers."

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
validate :: Input -> Either String Input
validate input
  | any (<= 0) [lowest input, highest input] =
    Left "Lowest & highest channels must be > 0."
  | lowest input > highest input =
    Left "Lowest channel must <= highest channel."
  | any (`notElem` crange) $ blocked input =
    Left "Blocked channels must be between lowest and highest, inclusive."
  | any (`notElem` crange) $ viewables input =
    Left "Viewable channels must be between lowest and highest, inclusive."
  | any (`elem` viewables input) $ blocked input =
    Left "Viewable channels can not be in the blocked channels list."
  | otherwise = Right input
  where crange = [lowest input .. highest input]

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type ParseError = String
type ParsedInput = String
minimumClicksIO :: FilePath -> IO (Either ParseError (ParsedInput, Clicks))
minimumClicksIO fp = do
    status <- isValidFilePath
    if status then do
        result <- parse fp
        case result of
          Right pInput -> return $ Right (show pInput, runReader minimumClicks pInput)
          Left err     -> return $ Left $ "File parse error: " ++ err
    else return $ Left $ " file '" ++ fp ++ "' should exist and must have .txt extension."
  where isValidFilePath :: IO Bool
        isValidFilePath = do
          exists <- doesFileExist fp
          let hasExtension = takeExtension fp == ".txt"
          return (exists && hasExtension)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
main :: IO ()
main = do
  -- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  putStrLn "+++ Game of Clicks -- Minimum Clicks For Viewable Channels Navigation."
  (_, failCount) <- foldM runTest (0, 0) testCases
  putStrLn ("+++ " ++ show failCount ++ " TEST FAILURES")
  where runTest :: (Int, Int) -> (FilePath, String) -> IO (Int, Int)
        runTest (pCount, fCount) (fp, expected) = do
          result <- minimumClicksIO fp
          let (pInput, actual) = case result of
                Right (x, y)  -> (x, show y)
                Left err      -> ("", err)
          putStrLn $ formatTestCase fp pInput actual expected
          case () of
            _ | actual == expected -> return (pCount + 1, fCount)
              | otherwise          -> return (pCount, fCount + 1)

testCases :: [(FilePath, String)]
testCases = [ ("./tests/good-inputs/normal.txt", "8")
            ,
              ("./tests/good-inputs/zero-viewables.txt", "0")
            ,
              ("./tests/good-inputs/zero-blocked.txt", "8")
            ,
              ("./tests/good-inputs/high-low-blocked-with-looping.txt", "10")
            ,
              ("./tests/good-inputs/same-high-low-blocked.txt", "0")
            ,
              ("./tests/good-inputs/same-high-low-viewable.txt", "1")
            ,
              ("./tests/good-inputs/blocked-within.txt", "8")
            ,
              ( "./tests/bad-inputs/missing-viewables.txt"
              , "File parse error: Specified viewables count /= # of viewable channels."
              )
            ,
              ( "./tests/bad-inputs/no-viewables-count.txt"
              , "File parse error: No count of viewables found."
              )
            ,
              ( "./tests/bad-inputs/empty-file.txt"
              , "File parse error: Input file must have 4 or more lines."
              )
            ]

formatTestCase :: FilePath -> String -> String -> String -> String
formatTestCase testFile input actual expected =
    let status  = if actual == expected then "PASS" else "FAIL"
    in "  ++  " ++ "Test File:  '"  ++ testFile ++ "'\n"  ++
       "      " ++ "Input:       "  ++ input ++ "\n"      ++
       "      " ++ "Actual:      "  ++ actual ++ "\n"     ++
       "      " ++ "Expected:    "  ++ expected ++ "\n"   ++
       "      " ++ "Status:      "  ++ status

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
