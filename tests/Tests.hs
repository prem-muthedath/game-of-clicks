-- | Contains all tests.
-- Author: Prem Muthedath
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Tests where

import Types (FileTest, InputTest)
import GameOfClicks (Input (..))
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- | Tests for `minimum clicks` computation.
-- See ./docs/problem-statement.txt to learn about Game-Of-Clicks & file format.
clicksTests :: [FileTest]
clicksTests =
  [ ("./tests/good-file-inputs/normal.txt", "8")
  ,
    ("./tests/good-file-inputs/zero-viewables.txt", "0")
  ,
    ("./tests/good-file-inputs/zero-blocked.txt", "8")
  ,
    ("./tests/good-file-inputs/high-low-blocked-with-looping.txt", "10")
  ,
    ("./tests/good-file-inputs/same-high-low-blocked.txt", "0")
  ,
    ("./tests/good-file-inputs/same-high-low-viewable.txt", "1")
  ,
    ("./tests/good-file-inputs/blocked-within.txt", "8")
  ,
    ( "./tests/bad-file-inputs/missing-blocked.txt"
    , "Parse failure in file './tests/bad-file-inputs/missing-blocked.txt':\
        \ Specified blocked count /= # of blocked channels."
    )
  ,
    ( "./tests/bad-file-inputs/missing-viewables.txt"
    , "Parse failure in file './tests/bad-file-inputs/missing-viewables.txt':\
        \ Specified viewables count /= # of viewable channels."
    )
  ,
    ( "./tests/bad-file-inputs/no-viewables-count.txt"
    , "Parse failure in file './tests/bad-file-inputs/no-viewables-count.txt':\
        \ No count of viewables found."
    )
  ,
    ( "./tests/bad-file-inputs/empty-file.txt"
    , "Parse failure in file './tests/bad-file-inputs/empty-file.txt':\
        \ Input file must have 4 or more lines."
    )
  ,
    ( "./tests/bad-file-inputs/non-text-file.xml"
    , "Parse failure in file './tests/bad-file-inputs/non-text-file.xml':\
        \ Missing or non '.txt' file."
    )
  ,
    ( "./tests/bad-file-inputs/non-integer-input.txt"
    , "Parse failure in file './tests/bad-file-inputs/non-integer-input.txt':\
        \ Input file must contain only integers."
    )
  ,
    ( "./tests/bad-file-inputs/negative-input.txt"
    , "Parse failure in file './tests/bad-file-inputs/negative-input.txt':\
        \ Input data must all be integers >= 0."
    )
  ]

-- | Tests to check if input file parse is done as expected.
-- See ./docs/problem-statement.txt to learn about Game-Of-Clicks & file format.
fileParseTests :: [FileTest]
fileParseTests =
  [ ("./tests/good-file-inputs/normal.txt"
    , "Input {lowest = 1, highest = 20, blocked = [18,19],\
        \ viewables = [15,14,17,11,17]}"
    )
  ,
    ("./tests/good-file-inputs/zero-viewables.txt"
    , "Input {lowest = 1, highest = 20,\
        \ blocked = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],\
        \ viewables = []}"
    )
  ,
    ("./tests/good-file-inputs/zero-blocked.txt"
    , "Input {lowest = 1, highest = 20, blocked = [],\
      \ viewables = [11,13,4,12,4]}"
    )
  ,
    ("./tests/good-file-inputs/high-low-blocked-with-looping.txt"
    , "Input {lowest = 1, highest = 20, blocked = [1,20],\
      \ viewables = [2,19,18,3,14,19,2,19]}"
    )
  ,
    ("./tests/good-file-inputs/same-high-low-blocked.txt"
    , "Input {lowest = 1, highest = 1, blocked = [1], viewables = []}"
    )
  ,
    ("./tests/good-file-inputs/same-high-low-viewable.txt"
    , "Input {lowest = 1, highest = 1, blocked = [], viewables = [1]}"
    )
  ,
    ("./tests/good-file-inputs/blocked-within.txt"
    , "Input {lowest = 1, highest = 20, blocked = [13,14,16],\
      \ viewables = [15,12,15,17,12,8]}"
    )
  ,
    ("./tests/bad-file-inputs/blocked-viewables.txt"
    , "\"Viewable channels can not be in the blocked channels list.\""
    )
  ,
    ("./tests/bad-file-inputs/zero-channel-numbers.txt"
    , "\"Lowest & highest channels must be > 0.\""
    )
  ]

inputTests :: [InputTest]
inputTests =
  [ ( Input 1 20 [18, 19] [15, 14, 17, 11, 17]
    , "8"
    )
  , ( Input 1 20 [18, 200] [3, 7]
    , "Blocked channels must be between lowest and highest channels, inclusive."
    )
  , ( Input 11 10 [3, 5] [6, 7]
    , "Lowest channel must be <= highest channel."
    )
  , ( Input 1 20 [18, 19] [15, 70]
    , "Viewable channels must be between lowest and highest channels, inclusive."
    )
  ]
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
