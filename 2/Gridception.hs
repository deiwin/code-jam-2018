module Gridception where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.List
import Data.Array.IArray
import Control.Monad

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

markNeighbouringMatches :: (MIx -> Bool) -> Table -> Table
markNeighbouringMatches matches table = accum (flip const) table $ do
    ix <- range $ bounds table
    guard $ table!ix == '.'
    let (i, j) = ix
    (di, dj) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
    let newIx = (i+di, j+dj)
    guard $ inRange (bounds table) newIx && table!newIx /= '.' && matches newIx
    return (newIx, '.')


markAllMatches :: (MIx -> Bool) -> Table -> Table
markAllMatches matches table =
    let markedTable = markNeighbouringMatches matches table
 in if markedTable == table
       then table
       else markAllMatches matches markedTable

countMatches' :: Table -> Int
countMatches' table = foldl' (\s ix -> if table!ix == '.' then s + 1 else s) 0 $ range $ bounds table
countMatches :: Table -> MIx -> (MIx -> Bool) -> Int
countMatches table searchRootIx matches = if matches searchRootIx
                                             then countMatches' $ markAllMatches matches $ table//[(searchRootIx, '.')]
                                             else 0

checkTable :: Table -> Int
checkTable table = maximum $ do
    let b = bounds table
    let rowCount = fst $ snd b
    let colCount = snd $ snd b
    pivotIx <- range b
    guard $ fst pivotIx /= rowCount && snd pivotIx /= colCount
    colorQuadrants <- combinations "WB" 4
    let matches ix = table!ix == expectedColor colorQuadrants pivotIx ix
    searchRootIx <- range b
    return $ countMatches table searchRootIx matches

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    (rows:cols:_) <- map read . words <$> await
    table <- parseTable <$> replicateM rows await
    let solution = show $ if rows == 1 && cols == 1
                             then 1
                             else checkTable table
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
