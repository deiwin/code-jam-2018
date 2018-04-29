import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Ratatouille hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "6",
          "2 1",
          "500 300",
          "900",
          "660",
          "2 1",
          "500 300",
          "1500",
          "809",
          "2 2",
          "50 100",
          "450 449",
          "1100 1101",
          "2 1",
          "500 300",
          "300",
          "500",
          "1 8",
          "10",
          "11 13 17 11 16 14 12 18",
          "3 3",
          "70 80 90",
          "1260 1500 700",
          "800 1440 1600",
          "1700 1620 900"
      ] >-> solve) @?= [
          "Case #1: 1",
          "Case #2: 0",
          "Case #3: 1",
          "Case #4: 0",
          "Case #5: 3",
          "Case #6: 3"
      ]
  ]
