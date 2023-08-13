module Game.Common where

import Game.Types (Input (..))
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