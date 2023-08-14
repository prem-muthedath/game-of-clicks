-- | Defines types used in testing.
-- Author: Prem Muthedath
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module Types where

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type Expected = String
type Actual = String
type Status = String              -- Usual test status: PASS/FAIL
type Test = (FilePath, Expected)  -- `FilePath` here is test input file path.

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
