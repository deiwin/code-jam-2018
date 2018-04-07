import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import SavingUniverse hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "6",
          "1 CS",
          "2 CS",
          "1 SS",
          "6 SCCSSC",
          "2 CC",
          "3 CSCSS"
      ] >-> solve) @?= [
         "Case #1: 1",
         "Case #2: 0",
         "Case #3: IMPOSSIBLE",
         "Case #4: 2",
         "Case #5: 0",
         "Case #6: 5"
      ]
  ]
