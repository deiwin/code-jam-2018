module Gridception where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.List
import Data.Array.IArray
import Control.Monad

import Data.Graph (stronglyConnComp, flattenSCC)

type MBounds = ((Int, Int), (Int, Int))
type MIx = (Int, Int)

type Table = Array MIx Char

parseTable :: [String] -> Table
parseTable rows = array bounds assocList
    where colCount = length $ head rows
          rowCount = length rows
          bounds = ((1, 1), (rowCount, colCount))
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

largestMatchingSubset :: Table -> (MIx -> Bool) -> Int
largestMatchingSubset table matches = maximum (0:connCompSizes)
    where connCompSizes = map (length . flattenSCC) $ stronglyConnComp graph
          graph = map createNode $ filter matches $ range $ bounds table
          createNode ix = ((), ix, map (addIx ix) [(-1, 0), (1, 0), (0, -1), (0, 1)])

checkTable :: Table -> Int
checkTable table = maximum $ do
    pivotIx <- range $ bounds table
    colorQuadrants <- combinations "WB" 4
    let matches ix = table!ix == expectedColor colorQuadrants pivotIx ix
    return $ largestMatchingSubset table matches

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    (rows:cols:_) <- map read . words <$> await
    table <- parseTable <$> replicateM rows await
    let solution = show $ checkTable table
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
