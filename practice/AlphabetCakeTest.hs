import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import AlphabetCake hiding (main)

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "given examples" $
      Pipes.toList (each [
          "4",
          "3 3",
          "G??",
          "?C?",
          "??J",
          "3 4",
          "CODE",
          "????",
          "?JAM",
          "2 2",
          "CA",
          "KE",
          "4 4",
          "G???",
          "????",
          "??C?",
          "???J"
      ] >-> solve) @?= [
          "Case #1:",
          "GCJ",
          "GCJ",
          "GCJ",
          "Case #2:",
          "CODE",
          "CJAM",
          "CJAM",
          "Case #3:",
          "CA",
          "KE",
          "Case #4:",
          "GCCJ",
          "GCCJ",
          "GCCJ",
          "GCCJ"
      ]
  , testCase "parseTable" $ do
      parseTable 1 3 ["G??"] @?= listArray ((1, 1), (1, 3)) [Just 'G', Nothing, Nothing]
      parseTable 1 3 ["GG?"] @?= listArray ((1, 1), (1, 3)) [Just 'G', Just 'G', Nothing]
      parseTable 1 3 ["GA?"] @?= listArray ((1, 1), (1, 3)) [Just 'G', Just 'A', Nothing]
      parseTable 1 3 ["G?A"] @?= listArray ((1, 1), (1, 3)) [Just 'G', Nothing, Just 'A']
      parseTable 3 3 ["G??", "?C?", "??J"] @?=
          listArray ((1, 1), (3, 3)) [ Just 'G', Nothing, Nothing
                                     , Nothing, Just 'C', Nothing
                                     , Nothing, Nothing, Just 'J'
                                     ]
  ]
