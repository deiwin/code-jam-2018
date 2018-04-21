import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import BitParty hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "4",
          "2 2 2",
          "1 2 3",
          "1 1 2",
          "2 2 2",
          "1 2 3",
          "2 1 2",
          "3 4 5",
          "2 3 3",
          "2 1 5",
          "2 4 2",
          "2 2 4",
          "2 5 1",
          "3 4 5",
          "9 4 2",
          "9 1 6",
          "9 4 2",
          "9 4 2",
          "9 4 2"
      ] >-> solve) @?= [
          "Case #1: 5",
          "Case #2: 4",
          "Case #3: 7",
          "Case #4: 8"
      ]
  , testCase "capacity" $ do
      capacity 10 (Cashier 3 4 5) @?= 1
      capacity 10 (Cashier 3 4 2) @?= 2
      capacity 10 (Cashier 1 4 2) @?= 1
  , testCase "binarySearch" $
      binarySearch (>= 7) 0 100 @?= 7
  ]
