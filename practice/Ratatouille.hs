module Ratatouille where

import Pipes
import qualified Pipes.Prelude as Pipes

import Control.Monad
import Data.Maybe
import Data.List

type Range = (Int, Int)

calculateRange :: Int -> Int -> Maybe Range
calculateRange requirement actual =
    let (low, lowRem) = (10 * actual) `quotRem` (11 * requirement)
        (high, highRem) = (10 * actual) `quotRem` (9 * requirement)
        lowFinal = if lowRem == 0 then low else low + 1
     in if lowFinal <= high
           then Just (lowFinal, high)
           else Nothing

calculateRanges' :: Int -> [Int] -> [Range]
calculateRanges' requirement = mapMaybe (calculateRange requirement)
calculateRanges :: [Int] -> [[Int]] -> [[Range]]
calculateRanges = zipWith calculateRanges'

readPackages :: Monad m => Int -> Pipe String String m [[Int]]
readPackages ingredientCount = map (map read . words) <$> replicateM ingredientCount await

rangesIntersect :: [Range] -> Bool
rangesIntersect ranges =
    let low = maximum $ map fst ranges
        high = minimum $ map snd ranges
     in low <= high

removeFirst :: Int -> [[Range]] -> [[Range]]
removeFirst highToRemove [] = error "We should find something to remove"
removeFirst highToRemove (first:rest)
  | [] <- first = first:removeFirst highToRemove rest
  | ((_, high):restPackages) <- first = if high == highToRemove
                                           then restPackages:rest
                                           else first:removeFirst highToRemove rest

countKits' :: Int -> [[Range]] -> Int
countKits' kits ranges
  | any null ranges = kits
  | rangesIntersect (map head ranges) = countKits' (kits + 1) (map tail ranges)
  | otherwise =
      let minHigh = minimum $ map (snd . head) ranges
       in countKits' kits (removeFirst minHigh ranges)

countKits :: [[Range]] -> Int
countKits ranges = countKits' 0 $ map sort ranges

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (ingredientCount:packageCount:_) = map read $ words line
    line2 <- await
    let ingredientRequirements = map read $ words line2
    packages <- readPackages ingredientCount
    let ranges = calculateRanges ingredientRequirements packages
    let solution = countKits ranges
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ show solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
