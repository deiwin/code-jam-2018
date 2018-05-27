import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Gridception hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "5",
          "3 3",
          "BBB",
          "BWB",
          "BBB",
          "2 3",
          "BBB",
          "WBW",
          "1 1",
          "W",
          "3 3",
          "WBW",
          "BWB",
          "WBW",
          "2 4",
          "BBWW",
          "BBWW"
      ] >-> solve) @?= [
          "Case #1: 8",
          "Case #2: 5",
          "Case #3: 1",
          "Case #4: 4",
          "Case #5: 8"
      ]
    , testCase "expectedColor" $ do
        expectedColor "BBBB" (1, 1) (1, 1) @?= 'B'
        let b = ((1, 1), (2, 4))
        array b (zip (range b) $ map (expectedColor "BWBW" (1, 2)) $ range b) @?=
            array b [ ((1, 1), 'B')
                    , ((1, 2), 'B')
                    , ((1, 3), 'W')
                    , ((1, 4), 'W')
                    , ((2, 1), 'B')
                    , ((2, 2), 'B')
                    , ((2, 3), 'W')
                    , ((2, 4), 'W')
                    ]
        array b (zip (range b) $ map (expectedColor "BBBB" (1, 2)) $ range b) @?=
            array b [ ((1, 1), 'B')
                    , ((1, 2), 'B')
                    , ((1, 3), 'B')
                    , ((1, 4), 'B')
                    , ((2, 1), 'B')
                    , ((2, 2), 'B')
                    , ((2, 3), 'B')
                    , ((2, 4), 'B')
                    ]
  ]
