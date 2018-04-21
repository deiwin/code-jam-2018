import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import BitParty hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, putSortedByLeastTimeForBitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "3",
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
          "2 5 1"
      ] >-> solve) @?= [
          "Case #1: 5",
          "Case #2: 4",
          "Case #3: 7"
      ]
  , testCase "sortCashiersByLeastTimeForBit" $ do
      sortCashiersByLeastTimeForBit [cashierWithTime 3, cashierWithTime 4, cashierWithTime 2] @?=
          [cashierWithTime 2, cashierWithTime 3, cashierWithTime 4]
      sortCashiersByLeastTimeForBit [cashierWithTime 4, cashierWithTime 3, cashierWithTime 2] @?=
          [cashierWithTime 2, cashierWithTime 3, cashierWithTime 4]
  ]

cashierWithTime :: Int -> Cashier
cashierWithTime time = Cashier { timeWithNextBit=time
                               , bitCapacity=0
                               , maxBits = 0
                               , scanTime=0
                               }

makePutSortedByLeastTimeForBitTest :: (Int, [Int], [Int]) -> TestTree
makePutSortedByLeastTimeForBitTest (val, list, newList) = testCase (show val ++ " into " ++ show list) $
    putSortedByLeastTimeForBit  (cashierWithTime val) (map cashierWithTime list) @?=
        map cashierWithTime newList

putSortedByLeastTimeForBitTests = testGroup "putSortedByLeastTimeForBit" $ map makePutSortedByLeastTimeForBitTest
    [ (1, [2, 3, 4], [1, 2, 3, 4])
    , (2, [1, 3, 4], [1, 2, 3, 4])
    , (2, [2, 3, 4], [2, 2, 3, 4])
    , (3, [1], [1, 3])
    ]
