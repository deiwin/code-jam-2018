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
  ]
