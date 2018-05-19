module FallingBalls where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array.IArray
import Data.List
import Control.Monad
import Data.Foldable
import Control.Applicative

type Acc = [Int]

type MBounds = ((Int, Int), (Int, Int))
type MIx = (Int, Int)
type Table = Array MIx Char

countRows :: Acc -> (Int, Int) -> Acc
countRows topCols (colIdx, ballsInCol) = replicate ballsInCol colIdx ++ topCols

createGrid :: [Int] -> Int -> Table
createGrid resColDifferences rows = accumArray (\a b -> b) initial bounds assocList
    where cols = length resColDifferences
          initial = '.'
          bounds = ((0, 0), (rows - 1, cols - 1))
          assocList = do
              (j, diff) <- zip [0..] resColDifferences
              let (c, diffs) | diff == 0 = ('.', []) | diff > 0 = ('\\', map (+ (-1)) [1..diff]) | otherwise = ('/', map (+ 1) $ reverse [diff..(-1)])
              (i, jd) <- zip [0..] diffs
              return ((i, j + jd), c)

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto columns list = let (head, rest) = splitAt columns list
                  in head:splitInto columns rest

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    colCount <- read <$> await
    ballsInCols <- map read . words <$> await
    let resColIdxs = reverse $ foldl' countRows [] (zip [0..] ballsInCols)
    let resColDifferences = uncurry (-) <$> zip resColIdxs [0..]
    let rows = 1 + maximum (map abs resColDifferences)
    let solution = if head ballsInCols == 0 || last ballsInCols == 0
                      then "IMPOSSIBLE"
                      else show rows
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    when (solution /= "IMPOSSIBLE") $ traverse_ yield $ splitInto colCount $ toList $ createGrid resColDifferences rows
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn