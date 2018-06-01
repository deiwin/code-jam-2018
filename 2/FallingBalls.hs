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

incrementsForDelta :: Int -> (Char, [Int])
incrementsForDelta diff
  | diff == 0 = ('.', [])
  | diff > 0 = ('\\', [0..(diff - 1)])
  | otherwise = ('/', reverse [(diff + 1)..0])

createGrid :: [Int] -> Int -> Table
createGrid resColDifferences rows = accumArray (flip const) initial bounds assocList
    where cols = length resColDifferences
          initial = '.'
          bounds = ((0, 0), (rows - 1, cols - 1))
          assocList = do
              (j, diff) <- zip [0..] resColDifferences
              let (c, diffs) = incrementsForDelta diff
              (i, jd) <- zip [0..] diffs
              return ((i, j + jd), c)

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto columns list = let (head, rest) = splitAt columns list
                  in head:splitInto columns rest
printTable :: Monad m => Table -> Pipe String String m ()
printTable table = traverse_ yield rowList
    where rowList = splitInto colCount $ toList table
          colCount = 1 + abs (snd (fst b) - snd (snd b))
          b = bounds table

solveCase :: Monad m => Int -> Pipe String String m ()
solveCase testCase = do
    colCount <- await
    ballsInCols <- map read . words <$> await
    let resColIdxs = reverse $ foldl' countRows [] (zip [0..] ballsInCols)
    let resColDifferences = uncurry (-) <$> zip resColIdxs [0..]
    let rows = 1 + maximum (map abs resColDifferences)
    let solution = if head ballsInCols == 0 || last ballsInCols == 0
                      then "IMPOSSIBLE"
                      else show rows
    yield $ "Case #" ++ show testCase ++ ": " ++ solution
    when (solution /= "IMPOSSIBLE") $ printTable $ createGrid resColDifferences rows

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCases <- read <$> await
    traverse_ solveCase [1..nrOfTestCases]

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
