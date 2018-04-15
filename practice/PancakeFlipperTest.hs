import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import PancakeFlipper hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "3",
          "---+-++- 3",
          "+++++ 4",
          "-+-+- 4"
      ] >-> solve) @?= [
         "Case #1: 3",
         "Case #2: 0",
         "Case #3: IMPOSSIBLE"
      ]
  , testCase "toIsHappy" $ do
      toIsHappy '+' @?= True
      toIsHappy '-' @?= False
  , testCase "needsFlip" $ do
      needsFlip True @?= False
      needsFlip False @?= True
  , testCase "canFlip" $ do
      canFlip 6 (take 5 $ repeat True) @?= False
      canFlip 5 (take 5 $ repeat True) @?= True
      canFlip 4 (take 5 $ repeat True) @?= True
  , testCase "flipN" $ do
      flipN 0 [True, True, True] @?= [True, True, True]
      flipN 1 [True, True, True] @?= [False, True, True]
      flipN 2 [True, True, True] @?= [False, False, True]
      flipN 3 [True, True, True] @?= [False, False, False]
      flipN 1 [False, False, False] @?= [True, False, False]
  ]
