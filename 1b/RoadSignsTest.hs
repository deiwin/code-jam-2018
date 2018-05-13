import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import RoadSigns hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "3",
          "1",
          "1 1 1",
          "5",
          "2 7 12",
          "6 3 11",
          "8 10 1",
          "11 11 12",
          "13 9 14",
          "5",
          "1 3 3",
          "2 2 2",
          "3 1 1",
          "4 2 2",
          "5 3 3"
      ] >-> solve) @?= [
          "Case #1: 1 1",
          "Case #2: 3 2",
          "Case #3: 5 1"
      ]
  ]
