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
  ]
