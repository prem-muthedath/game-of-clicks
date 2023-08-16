#### game-of-clicks

In this game, we aim to find the minimum number of clicks to navigate a sequence 
of viewable TV channels, given a channel range and other constraints.

To learn more about the game, see: ./docs/problem-statement.txt

##### Author: Prem Muthedath

##### GHC version: 8.10.4, cabal 3.40

##### How to use the library, the app, & run the tests:
  1. `cd` to `game-of-clicks`, the top directory containing this `README` file, 
     and remain there to execute any of the steps below.
  2. To load the `GameOfClicks` library into `GHCi`, enter the below command at 
     the commandline:

     ````
     cabal v2-repl :game-of-clcks
     ````

     You can then use the library in `GHCi` to compute the minumum clicks to 
     navigate a viewable sequence of TV channels.  For example, given an input 
     file (NOTE: all input files should have `.txt` extension) having the game 
     data, you can enter at the `GHCi` prompt the following command:

     ````
     minimumClicksIO "./tests/good-file-inputs/normal.txt"
     ````

     Or you could use another function in the library that takes the game data 
     as arguments (instead of an input file) to compute the minimum clicks.

     ````
     minimumClicksFor 1 20 [18, 19] [15, 14, 17,11, 17]
     ````

  3. If you wish to run the tests on the unix commandline, you can do so by 
     running the below command (NOTE: If you are in `GHCi`, first exit from 
     there):

     ````
     cabal v2-run -f testing :game-of-clicks-tests
     ````

  4. If you wish to run the tests in `GHCi` instead, you can do so by first 
     starting `GHCi` by loading the tests and then running those tests, using 
     the below 2 commands:

     ````
     1. cabal v2-repl -f testing :game-of-clicks-tests
     2. main
     ````

  5. If you wish to run the `app` on the unix commandline, you can do so in one 
     of the two following ways, as the example below that uses an input file (by 
     the way, all input files should have `.txt` extension) shows (NOTE: If you 
     are in `GHCi`, first exit from there):

     ````
     cabal v2-run :game-of-clicks-app -- "tests/good-file-inputs/normal.txt"`
     cabal v2-run :game-of-clicks-app "tests/good-file-inputs/normal.txt"`
     ````

  6. If you would like to use the `app` in `GHCi` instead, you can do so by 
     first starting `GHCi` by loading the `app` and then running the `app`, as 
     the example below that uses an input file (it has be a `.txt` file) shows:

     ````
     1. cabal v2-repl :game-of-clicks-app
     2. :set args "./tests/good-file-inputs/normal.txt"
     3. main
     ````
