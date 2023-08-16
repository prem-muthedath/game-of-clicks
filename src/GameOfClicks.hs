{-# LANGUAGE CPP #-}
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Game Of Clicks (R->Hard)
--
-- Briefly, in this game, we aim to find the minimum number of clicks to 
-- navigate a sequence of viewable TV channels, given a channel range and other 
-- constraints.
--
-- To learn about the game, see:
--  1. ./docs/problem-statement.txt
--  2. https://prepinsta.com/thoughtworks-coding-questions/
--
-- Haskell Solution.
-- Author: Prem Muthedath, AUG 2023.
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- REF on CPP directive:
--  1. https://guide.aelve.com/haskell/cpp-vww0qd72 (CPP overview)
--  2. https://tinyurl.com/hr2x3n8r (/u/ ralph, so) (CPP in module export)
--  3. https://tinyurl.com/2s4h9vth (cs.auckland.nz, #if, #ifdef, #endif)
--  4. https://tinyurl.com/33wmdvzd (/u/ willen van onsem, so) (CPP indentation)
module GameOfClicks
  (
  -- * Core functions for minimum clicks computations
    minimumClicksIO
  , minimumClicksFor
#ifdef TESTING
  -- ** Input file parse -- access restricted to internal library tests.
  , parse
#endif
  -- * Types
  , Clicks
  , Channel
  , Lowest
  , Highest
  , Blocked
  , Viewable
#ifdef TESTING
  -- ** Input record -- access restricted to internal library tests
  , Input (..)
#endif
  -- ** File parse error type and its accessors
  , FileParseError
  , errFilePath
  , errMessage
  ) where

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
import Game.IO
import Game.Core (minimumClicksFor)
import Game.Types

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
