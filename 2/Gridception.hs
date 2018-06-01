module Gridception where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array.IArray
import Control.Monad
import Data.Foldable

import Data.Graph (stronglyConnComp, flattenSCC)

type MBounds = ((Int, Int), (Int, Int))
type MIx = (Int, Int)

type Table = Array MIx Char

parseTable :: Int -> Int -> [String] -> Table
parseTable rowCount colCount rows = array bounds assocList
    where bounds = ((1, 1), (rowCount, colCount))
          assocList = do
              (i, row) <- zip [1..] rows
              (j, char) <- zip [1..] row
              return ((i, j), char)

combinations :: [a] -> Int -> [[a]]
combinations xs n = mapM (const xs) [1..n]

expectedColor :: String -> MIx -> MIx -> Char
expectedColor quadrants (pivotI, pivotJ) (i, j)
  | i <= pivotI && j <= pivotJ = head quadrants
  | i <= pivotI = quadrants!!1
  | j <= pivotJ = quadrants!!2
  | otherwise = quadrants!!3

addIx :: MIx -> MIx -> MIx
addIx (i, j) (di, dj) = (i + di, j + dj)

largestMatchingSubset :: Table -> Int -> (MIx -> Bool) -> Int
largestMatchingSubset table m matches = maxUpTo m (0:connCompSizes)
    where connCompSizes = map (length . flattenSCC) $ stronglyConnComp graph
          graph = map createNode $ filter matches $ indices table
          createNode ix = ((), ix, map (addIx ix) [(-1, 0), (1, 0), (0, -1), (0, 1)])

checkTable :: Table -> Int -> Int
checkTable table m = maxUpTo m $ do
    pivotIx <- indices table
    colorQuadrants <- combinations "WB" 4
    let matches ix = table!ix == expectedColor colorQuadrants pivotIx ix
    return $ largestMatchingSubset table m matches

maxUpTo :: Ord a => a -> [a] -> a
maxUpTo m xs = case span (< m) xs of (_, []) -> maximum xs
                                     _ -> m

solveCase :: Monad m => Int -> Pipe String String m ()
solveCase testCase = do
    (rows:cols:_) <- map read . words <$> await
    table <- parseTable rows cols <$> replicateM rows await
    let solution = show $ checkTable table (rows * cols)
    yield $ "Case #" ++ show testCase ++ ": " ++ solution

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCases <- read <$> await
    traverse_ solveCase [1..nrOfTestCases]

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
