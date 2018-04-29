import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import RoundingError hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "4",
          "3 2",
          "1 1",
          "10 3",
          "1 3 2",
          "6 2",
          "3 1",
          "9 8",
          "1 1 1 1 1 1 1 1"
      ] >-> solve) @?= [
          "Case #1: 100",
          "Case #2: 100",
          "Case #3: 101",
          "Case #4: 99"
      ]
  ]
