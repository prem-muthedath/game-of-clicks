-- | Defines all types used in `game-of-clicks` library.
-- Author: Prem Muthedath
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Game.Types where

import Control.Monad.Reader (Reader)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- First, define some type synonyms.
-- See ./docs/problem-statement.txt to learn about Game-Of-Clicks terminology.
type Clicks = Int; type Channel = Int   -- TV Channel
type Lowest = Channel; type Highest = Channel
type Blocked = Channel; type Viewable = Channel

-- | Define file parse error and its accessors.
data FileParseError = FileParseError FilePath String deriving Eq

-- | `Show` instance.`
instance Show FileParseError where
  show (FileParseError fp msg) = "Parse failure in file '" ++ fp ++ "': " ++ msg

-- | Extract path (includes file name) of file that generated this error.
errFilePath :: FileParseError -> FilePath
errFilePath (FileParseError fp _) = fp

-- | Extract the error message.
errMessage :: FileParseError -> String
errMessage (FileParseError _ msg) = msg

-- `Input` record to capture/store all input we need for this problem.
data Input = Input { lowest, highest :: Channel
                   , blocked, viewables :: [Channel]
                   } deriving (Show, Eq)

-- Type synonym for Reader monad usage.
type ClicksReader a = Reader Input a

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
