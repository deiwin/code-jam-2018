module Gridception where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.List
import Data.Array.IArray
import Control.Monad

import Data.Set (Set)
import qualified Data.Set as S

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

findConnected' bounds (i, j) (found, available) (di, dj) =
    let ix = (i + di, j + dj)
        same = (found, available)
 in if not (inRange bounds (i + di, j + dj))
       then same
       else let newAvailable = S.delete ix available
    in if S.size newAvailable == S.size available
          then same
          else let (additionalFound, remaining) = findConnected bounds ix newAvailable
       in (S.union found additionalFound, remaining)
findConnected :: MBounds -> MIx -> Set MIx -> (Set MIx, Set MIx)
findConnected bounds ix availableIxs = foldl' (findConnected' bounds ix) (S.singleton ix, availableIxs) [(-1, 0), (1, 0), (0, -1), (0, 1)]

connectedSubsets :: MBounds -> Set MIx -> [Set MIx]
connectedSubsets bounds ixs
  | null ixs = []
  | otherwise = subset:connectedSubsets bounds remaining
  where (subset, remaining) = findConnected bounds start withoutStart
        (start, withoutStart) = S.deleteFindMin ixs

matchingIxs :: (MIx -> Bool) -> MBounds -> Set MIx
matchingIxs matches bounds = S.fromDistinctAscList $ filter matches $ range bounds

largestMatchingSubset :: Table -> (MIx -> Bool) -> Int
largestMatchingSubset table matches = if null subsetSizes
                                         then 0
                                         else maximum subsetSizes
    where b = bounds table
          subsetSizes = map S.size $ connectedSubsets b $ matchingIxs matches b

checkTable :: Table -> Int
checkTable table = maximum $ do
    let b = bounds table
    let rowCount = fst $ snd b
    let colCount = snd $ snd b
    pivotIx <- range b
    guard $ fst pivotIx /= rowCount && snd pivotIx /= colCount
    colorQuadrants <- combinations "WB" 4
    let matches ix = table!ix == expectedColor colorQuadrants pivotIx ix
    return $ largestMatchingSubset table matches

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
