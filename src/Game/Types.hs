module Game.Types where

import Control.Monad.Reader (Reader)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- First, define some type synonyms.
type Clicks = Int; type Channel = Int

-- | Define file parse error and its accessors.
data FileParseError = FileParseError FilePath String

instance Show FileParseError where
  show (FileParseError fp msg) = "Parse failure in file '" ++ fp ++ "': " ++ msg

errFilePath :: FileParseError -> FilePath
errFilePath (FileParseError fp _) = fp

errMessage :: FileParseError -> String
errMessage (FileParseError _ msg) = msg

-- `Input` record to capture/store all input we need for this problem.
data Input = Input { lowest, highest :: Channel
                   , blocked, viewables :: [Channel]
                   } deriving (Show, Eq)

-- Type synonym for Reader monad usage.
type ClicksReader a = Reader Input a

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
