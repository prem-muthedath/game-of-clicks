-- | Defines types used in testing.
-- Author: Prem Muthedath
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Types where

import GameOfClicks (Input)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type TestInput = String
type Expected = String
type Actual = String
type Status = String                  -- Usual test status: PASS/FAIL
type FileTest = (FilePath, Expected)  -- `FilePath` here is test input file path.
type InputTest = (Input, Expected)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
