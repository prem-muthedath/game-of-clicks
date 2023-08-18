#### game-of-clicks

In this game, we aim to find the minimum number of clicks to navigate a sequence 
of viewable TV channels, given a channel range and other constraints.

To learn more about the game, see: `./docs/problem-statement.txt`

##### Author: Prem Muthedath

##### GHC version: 8.10.4, cabal 3.40

##### Project journey
How did the solution to the TV Channel navigation problem called the 
`game-of-clicks` emerge?  Well, at first it looked more complex than it really 
is, and you thought dynamic programming is needed.  Then, on closer inspection, 
you learned that the solution needs another, much simpler approach.  You then 
coded the stuff in Haskell, using the Reader monad, and it came out beautiful.  
How do you then turn this stuff into a cabal package with library, app, and 
tests?  Well, that required splitting up stuff into modules, defining types, 
deciding what to expose and what not to expose to clients, coming up with test 
data and edge cases, and conditional compilation using CPP.  Finally, you had to 
also roll out the documentation.  That's how the journey really unfolded.

##### How to use the library, the app, & run the tests:
  1. `cd` to `game-of-clicks`, the top directory containing this `README` file, 
     and remain there to execute any of the steps below.

  2. Some commands and library functions given below take a file as input, and 
     these input files are all text files, because `game-of-clicks` only accepts 
     text files with a `.txt` extension as input.  Also, all input files must be 
     in the specified format.

     See `./docs/problem-statement.txt` to learn about input file format 
     specification and input data constraints.  See also 
     `./tests/good-file-inputs/normal.txt` for a sample input file.

  3. To load the `GameOfClicks` library into `GHCi`, enter the below command on 
     the commandline:

     ````
     cabal v2-repl :game-of-clcks
     ````

     You can then use the library in `GHCi` to compute the minumum clicks to 
     navigate a viewable sequence of TV channels.  For example, given an input 
     file having game data, you can enter at the `GHCi` prompt the following 
     command:

     ````
     minimumClicksIO "./tests/good-file-inputs/normal.txt"
     ````

     Or you could use another function in the library that takes the game data 
     as arguments (instead of an input file) to compute the minimum clicks.

     ````
     minimumClicksFor 1 20 [18, 19] [15, 14, 17,11, 17]
     ````

  4. If you wish to run the tests on the unix commandline, you can do so by 
     running the below command (NOTE: If you are in `GHCi`, first exit from 
     there):

     ````
     cabal v2-run -f testing :game-of-clicks-tests
     ````

  5. If you wish to run the tests in `GHCi` instead, you can do so on the unix 
     commandline by first starting `GHCi` by loading the tests (step 1) and then 
     running those tests in `GHCi` (step 2), using the below 2 commands:

     ````
     1. cabal v2-repl -f testing :game-of-clicks-tests
     2. main
     ````

  6. If you wish to run the `app` on the unix commandline, you can do so in one 
     of the two following ways, as the example below that uses an input file 
     shows (NOTE: If you are in `GHCi`, first exit from there):

     ````
     cabal v2-run :game-of-clicks-app -- "tests/good-file-inputs/normal.txt"`
     cabal v2-run :game-of-clicks-app "tests/good-file-inputs/normal.txt"`
     ````

  7. If you would like to use the `app` in `GHCi` instead, you can do so on the 
     unix commandline by first starting `GHCi` by loading the `app` (step 1) and 
     then running the `app` in `GHCi` (steps 2 & 3), as the example below that 
     uses an input file shows:

     ````
     1. cabal v2-repl :game-of-clicks-app
     2. :set args "./tests/good-file-inputs/normal.txt"
     3. main
     ````
