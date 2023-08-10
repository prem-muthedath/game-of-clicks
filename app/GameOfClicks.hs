{-# LANGUAGE ScopedTypeVariables #-}
--
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Game Of Clicks (R->Hard)
-- thoughtworks interview problem
-- https://prepinsta.com/thoughtworks-coding-questions/
-- see also ./problem-statement.txt
--
-- Haskell Solution.
-- author: Prem Muthedath, AUG 2023.
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module GameOfClicks where

import Control.Monad.Reader (Reader, runReader, ask, asks, forM)
import Data.List (lines, nub)

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
    then do up    <- upNavigationChannels
            down  <- downNavigationChannels
            minimum <$> mapM totalClicks [up, down]
    else error $ show [from, to] ++ " has blocked channels"
  where upNavigationChannels :: ClicksReader [Channel]
        upNavigationChannels = do
          vHigh <- viewableHigh
          vLow  <- viewableLow
          case () of
            _ | from == vHigh -> return [vLow .. to]
              | from < to -> return [from + 1 .. to]
              | from > to -> return $ [from + 1 .. vHigh] ++ [vLow .. to]
              | otherwise -> return []
        downNavigationChannels :: ClicksReader [Channel]
        downNavigationChannels = do
          vHigh <- viewableHigh
          vLow  <- viewableLow
          case () of
            _ | from == vLow -> return [vHigh, vHigh - 1 .. to]
              | from < to -> return $ [from-1, from-2 .. vLow] ++ [vHigh, vHigh-1 .. to]
              | from > to -> return [from-1, from-2 .. to]
              | otherwise -> return []
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
          initial <- clicksToGetStarted
          input <- ask
          let vChannels = viewables input
              viewableTriples = zip3 (maxBound:vChannels) vChannels (tail vChannels)
          rest <- forM viewableTriples $
            \(prev, from, to) -> do
                let lastViewed :: Clicks = lastViewedClicks prev to
                    keyPressed :: Clicks = keyPressClicks to
                upDown :: Clicks <- upDownClicks from to
                return $ minimum [lastViewed, upDown, keyPressed]
          return (initial:rest)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Parse input data to create an `Input` record.
-- See ./docs/problem-statement.txt to learn about input format.
-- see ./test/game-of-clicks-input.txt for a sample input.
parse :: [Int] -> Input
parse xs
  | length xs < 4 = error "Input Data Must Have At Least 4 Elements For Parsing."
  | otherwise =
    let lowest'     = head xs
        highest'    = head $ tail xs
        bcount      = xs !! 2
        blocked'    = nub $ take bcount $ drop (2 + 1) xs
        vcount      = xs !! (2 + 1 + bcount)
        viewables'  = take vcount $ drop (2 + 1 + bcount + 1) xs
    in Input { lowest = lowest'
             , highest = highest'
             , blocked = blocked'
             , viewables = viewables'
             }

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
main :: IO ()
main = do
  let inputFile = "./test/game-of-clicks-input.txt"
  input :: [Int] <- fmap read . Data.List.lines <$> readFile inputFile
  let pInput :: Input = parse input
      minClicks :: Clicks = runReader minimumClicks pInput
  putStrLn $ "Game Of Clicks Input: " ++ show pInput
  putStrLn $ "Minimum Clicks For Viewable Channels Navigation: " ++ show minClicks

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
