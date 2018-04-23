import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import EdgyBaking hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

(@?~) :: (Eq a, Show a, Num a, Ord a, Fractional a) => a -> a -> Assertion
(@?~) actual expected =
    let epsilon = 10^^(-6)
        assertion = abs (actual - expected) <= epsilon ||
            abs ((actual - expected) / actual) <= epsilon ||
                abs ((actual - expected) / expected) <= epsilon
        message = "Expected " ++ show actual ++ " to be close to " ++ show expected
     in assertion @? message

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "4",
          "1 7",
          "1 1",
          "2 920",
          "50 120",
          "50 120",
          "1 32",
          "7 4",
          "3 240",
          "10 20",
          "20 30",
          "30 10"
      ] >-> solve) @?= [
          "Case #1: 6.828427",
          "Case #2: 920.000000",
          "Case #3: 32.000000",
          "Case #4: 240.000000"
      ]
  , testCase "addedRange" $ do
      fst (addedRange (1, 1)) @?~ 2
      snd (addedRange (1, 1)) @?~ 2.828427
      fst (addedRange (7, 4)) @?~ 8
      snd (addedRange (7, 4)) @?~ 16.124515
  , testCase "add" $
      add (1, 2) (3, 4) @?= (4, 6)
  , testCase "within" $ do
      within (1.0, 3.0) 0 @?= False
      within (1.0, 3.0) 1 @?= True
      within (1.0, 3.0) 2 @?= True
      within (1.0, 3.0) 3 @?= True
      within (1.0, 3.0) 4 @?= False
  , testCase "below" $ do
      below (1.0, 3.0) 0 @?= True
      below (1.0, 3.0) 1 @?= False
      below (1.0, 3.0) 2 @?= False
      below (1.0, 3.0) 3 @?= False
      below (1.0, 3.0) 4 @?= False
  ]
