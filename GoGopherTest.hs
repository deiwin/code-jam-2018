import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import GoGopher hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "1",
          "20",
          "2 2",
          "1 1",
          "8 9"
      ] >-> solve) @?= [
         "2 2",
         "2 2",
         "2 3"
      ]
  , testCase "emptyGrid + fill" $ do
      emptyGrid 1 2 @?= [[False, False]]
      emptyGrid 3 2 @?= [[False, False], [False, False], [False, False]]
      fill 1 1 (emptyGrid 3 2) @?= [[False, False], [False, True], [False, False]]
      filledAt 1 1 (fill 1 1 (emptyGrid 3 2)) @?= True
      filledAt 1 0 (fill 1 1 (emptyGrid 3 2)) @?= False
  ]
