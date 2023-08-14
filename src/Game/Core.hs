{-# LANGUAGE ScopedTypeVariables #-}
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Game-Of-Clicks (R->Hard) core module.
-- Handles all logic to compute minimum clicks needed to navigate a sequence of 
-- viewable channels.
--
-- To learn about the game, see:
--  1. ./docs/problem-statement.txt
--  2. https://prepinsta.com/thoughtworks-coding-questions/
--
-- Haskell Solution.
-- Author: Prem Muthedath, AUG 2023.
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Game.Core where

import Control.Monad.Reader (runReader, ask, asks, forM)

import Game.Types (ClicksReader, Input (..), Clicks, Channel)
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
    up    <- upNavigationClicks
    min up <$> downNavigationClicks
  where upNavigationClicks :: ClicksReader Clicks
        upNavigationClicks = do
          vLow  <- viewableLow
          vHigh <- viewableHigh
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
          vLow  <- viewableLow
          vHigh <- viewableHigh
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
