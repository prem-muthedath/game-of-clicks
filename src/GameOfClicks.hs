{-# LANGUAGE CPP #-}
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Game Of Clicks (R->Hard)
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
  ( minimumClicksIO
#ifdef TESTING
  , parse
#endif
  , Clicks
  , FileParseError
  , errFilePath
  , errMessage
  ) where

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
import Game.IO (minimumClicksIO, parse)
import Game.Types (Clicks, FileParseError, errFilePath, errMessage)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
