import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import TroubleSort hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, solveCaseTests]

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

makeSolveCaseTest :: ([Int], Maybe Int) -> TestTree
makeSolveCaseTest (list, result) = testCase (unwords (map show (take 10 list)) ++ " -> " ++ (show result)) $
    solveCase (length list) list @?= result

solveCaseTests = testGroup "solveCase" $ map makeSolveCaseTest
    [ ([5, 6, 8, 4, 3], Nothing)
    , ([8, 9, 7], Just 1)
    , ([-2, -1, 0, 1, 2, 4, 3, 5, 6, 7, 8], Just 5)
    , ([-1, 0, 1, 2, 4, 3, 5, 6, 7, 8], Just 4)
    , ([0, 1, 2, 4, 3, 5, 6, 7, 8], Just 3)
    , ([1, 2, 4, 3, 5, 6, 7, 8], Just 2)
    , ([2, 4, 3, 5, 6, 7, 8], Just 1)
    , ([4, 3, 5, 6, 7, 8], Just 0)
    , ((reverse [0..100000]), Nothing)
    , ([0..100000], Nothing)
    ]
