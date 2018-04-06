import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Solution hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "simple" $
      \x -> simple x == x
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "simple" $
      \x -> simple x == x
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  , testCase "simple" $
      simple 2 @?= 2
  , testCase "input,output" $
      Pipes.toList (each ["Hello, ", "world!"] >-> solve) @?= ["Hello, world!"]
  ]
