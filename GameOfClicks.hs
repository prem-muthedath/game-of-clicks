{-# LANGUAGE ScopedTypeVariables #-}

-- thoughtworks interview problem
-- https://prepinsta.com/thoughtworks-coding-questions/
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Game Of Clicks (R->Hard)
-- Problem Statement:
-- Sahil watches TV all day and gets bored. He started playing this dumb game of 
-- identifying minimum number of inputs needed to reach a channel. As his 
-- cousin, you have to help him, but you live far from his house. So you decide 
-- to write a code that will ask Sahil for some inputs and give outputs 
-- respectively.
--
-- Here are the problems you need to keep in mind:
--
-- There are 13 buttons on his remote: 10 buttons for the numbers (0-9) to form 
-- integers denoting respective channel index, “Up channel” button and “ Down 
-- channel” button for going i +1th channel and i-1th channel from i 
-- respectively, and a “Last viewed” button to see what’s the last channel 
-- before it.
-- The number buttons allow you to jump directly to a specific channel (Ex: to 
-- go to channel 172 by typing 1,7,2).
-- If the channel which you are in is ith and that is the max channel index 
-- possible, by Up channel, you will reach the first channel possible. Same goes 
-- for the down channel button. You can go to the highest channel possible if 
-- you go down from the lowest channel possible.
-- Sahil can get from one channel to the next in one of the two ways.
-- Sahil’s parents have set some parental control on some channels on Aniruth’s 
-- television. The “Up Channel “ and “Down buttons” buttons skip these channels 
-- as these channels are not viewable.
-- Given a list of channels to view, the lowest channel, the highest channel, 
-- and a list of blocked channels, your program should return the minimum number 
-- of clicks necessary to get through all the shows that Anirudh would like to 
-- match.
-- Input Format:
--
-- First line is the lowest Channel
-- Second-line is the highest Channel
-- Followed by a number of blocked channels B,
-- and the next B lines contain the actual blocked channels.
-- Followed by the number of Channels to view V, and the next V lines contain 
-- the actual channels to view.
-- Constraints :
--
-- The lowest channel on the television will be greater than 0. and less than or 
-- equal to 10,000.
-- The highest channel on the television will be greater than or equal to the 
-- lowest channel. and less than or equal to 10.000.
-- The list of channels that are blocked on Anirudh’s television. All the 
-- channels in this list will be valid channels (greater than or equal to lowest 
-- channel, less than or equal 1 to highest channel). Duplicates may be Ignored. 
-- The blocked list can be a maximum of 40 channels.
-- The sequence that Sahil must view contains between 1 and 50 elements. 
-- inclusive. All channels in this sequence are not in the blocked list and are 
-- between lowest channel and highest channel. Inclusive.
-- Sample Input 0:
-- 1
-- 20
-- 2
-- 18
-- 19
-- 5
-- 15
-- 14
-- 17
-- 1T
-- 17
-- Sample output 0:
-- 7
--
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Haskell Solution.
-- author: Prem Muthedath, AUG 2023.
--
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
import Data.List (lines)

-- first, define some types
type Clicks = Int; type Channel = Int
data Input = Input { lowest, highest :: Channel
                   , blocked, viewables :: [Channel]
                   } deriving Show

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
upDownClicks :: Input -> Channel -> Channel -> Clicks
upDownClicks input@Input {lowest = low, highest = high, blocked = blkd} from to
  | all (`notElem` blkd) [from, to]
    = minimum $ map totalClicks [upNavigationChannels, downNavigationChannels]
  | otherwise = error $ show [from, to] ++ " has blocked channels"
  where upNavigationChannels :: [Channel]
        upNavigationChannels
           | from == viewableHigh = [viewableLow .. to]
           | from < to = [from + 1 .. to]
           | from > to = [from + 1 .. viewableHigh] ++ [viewableLow  .. to]
           | otherwise = []
        downNavigationChannels :: [Channel]
        downNavigationChannels
           | from == viewableLow = [viewableHigh, viewableHigh - 1 .. to]
           | from < to = [from-1, from-2 .. viewableLow] ++ [viewableHigh, viewableHigh-1 .. to]
           | from > to = [from-1, from-2 .. to]
           | otherwise = []
        viewableLow :: Channel
        viewableLow
          | low `notElem` blkd = low
          | otherwise = head $ dropWhile (`elem` blkd) allChannels
        viewableHigh :: Channel
        viewableHigh
          | high `notElem` blkd = high
          | otherwise = head $ dropWhile (`elem` blkd) (reverse allChannels)
        allChannels :: [Channel]
        allChannels = [low .. high]
        totalClicks :: [Channel] -> Clicks
        totalClicks = length . filter (`notElem` blkd)

keyPressClicks :: Channel -> Clicks
keyPressClicks = length . show

lastViewedClicks :: Channel -> Channel -> Clicks
lastViewedClicks prev to  | prev == to = 1
                          | otherwise = maxBound

clicksToGetStarted :: Input -> Clicks
clicksToGetStarted Input {viewables = vChannels} = keyPressClicks $ head vChannels

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
minimumClicks :: Input -> Clicks
minimumClicks input@Input {viewables = vChannels}
  | null vChannels = 0
  | [x] <- viewables' = clicksToGetStarted input
  | otherwise = foldr (\(prev, from, to) acc ->
    let clicks = [ lastViewedClicks prev to
                 , upDownClicks input from to
                 , keyPressClicks to
                 ]
    in acc + minimum clicks)
    (clicksToGetStarted input) $
    zip3 (maxBound:vChannels) vChannels (tail vChannels)
  where viewables' = viewables input

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
parse :: [Int] -> Input
parse [] = error "No Input Data Found For Parsing."
parse xs =
  let lowest'     = head xs
      highest'    = head $ tail xs
      bcount      = xs !! 2
      blocked'    = take bcount $ drop (2 + 1) xs
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
  input :: [Int] <- fmap read . Data.List.lines <$> readFile "./GameOfClicksInput.txt"
  let pInput :: Input = parse input
      minClicks :: Clicks = minimumClicks pInput
  putStrLn $ "Game Of Clicks Input: " ++ show pInput
  putStrLn $ "Minimum Clicks For Viewable Channels Navigation: " ++ show minClicks

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

