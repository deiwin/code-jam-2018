import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import TroubleSort hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "2",
          "5",
          "5 6 8 4 3",
          "3",
          "8 9 7"
      ] >-> solve) @?= [
         "Case #1: OK",
         "Case #2: 1"
      ]
  ]
