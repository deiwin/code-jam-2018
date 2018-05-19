import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import FallingBalls hiding (main)

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
          "4",
          "1 1 1 1",
          "3",
          "0 2 1",
          "6",
          "3 0 0 2 0 1",
          "6",
          "1 0 0 3 0 2"
      ] >-> solve) @?= [
          "Case #1: 1",
          "....",
          "Case #2: IMPOSSIBLE",
          "Case #3: 3",
          ".//./.",
          "./....",
          "......",
          "Case #4: 3",
          ".\\\\.\\.",
          "..\\...",
          "......"
      ]
  ]
