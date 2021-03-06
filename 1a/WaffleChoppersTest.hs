import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import WaffleChoppers hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "6",
          "3 6 1 1",
          ".@@..@",
          ".....@",
          "@.@.@@",
          "4 3 1 1",
          "@@@",
          "@.@",
          "@.@",
          "@@@",
          "4 5 1 1",
          ".....",
          ".....",
          ".....",
          ".....",
          "4 4 1 1",
          "..@@",
          "..@@",
          "@@..",
          "@@..",
          "3 4 2 2",
          "@.@@",
          "@@.@",
          "@.@@",
          "3 4 1 2",
          ".@.@",
          "@.@.",
          ".@.@"
      ] >-> solve) @?= [
          "Case #1: POSSIBLE",
          "Case #2: IMPOSSIBLE",
          "Case #3: POSSIBLE",
          "Case #4: IMPOSSIBLE",
          "Case #5: POSSIBLE",
          "Case #6: IMPOSSIBLE"
      ]
  , testCase "hasChip" $ do
      hasChip '@' @?= True
      hasChip '.' @?= False
  , testCase "chipsInColumns" $ do
      chipsInColumns 3 [[1, 0, 0]] @?= [1, 0, 0]
      chipsInColumns 3 [[1, 0, 0], [1, 1, 0]] @?= [2, 1, 0]
  , testCase "chipsInRows" $ do
      chipsInRows [[1, 0, 0]] @?= [1]
      chipsInRows [[1, 0, 0], [1, 1, 0]] @?= [1, 2]
  , testCase "totalChips" $ do
      totalChips [[1, 0, 0]] @?= 1
      totalChips [[1, 0, 0], [1, 1, 0]] @?= 3
  , testCase "canDistribute" $ do
      canDistribute 2 [[2, 2], [2, 2]] @?= True
      canDistribute 3 [[2, 2], [2, 2]] @?= False
      canDistribute 2 [[3, 2], [2, 2]] @?= False
  ]
