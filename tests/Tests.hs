module Tests where

import Types (Test)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tests :: [Test]
tests =
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
  ]

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
