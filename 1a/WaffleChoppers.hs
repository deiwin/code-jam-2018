module WaffleChoppers where

import Pipes
import qualified Pipes.Prelude as Pipes

type HasChip = Bool
type Row = [HasChip]
type Grid = [Row]

hasChip :: Char -> HasChip
hasChip = (== '@')

readGrid' :: Monad m => Grid -> Int -> Pipe String String m Grid
readGrid' acc 0 = return acc -- acc should be reversed, but doesn't matter in our case
readGrid' acc rowsToRead = do
    line <- await
    let row = map hasChip line
    readGrid' (row:acc) (rowsToRead -1 )
readGrid :: Monad m => Int -> Pipe String String m Grid
readGrid = readGrid' []

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (rows:columns:vCuts:hCuts:_) = map read $ words line
    grid <- readGrid rows
    let solution = if True then "POSSIBLE" else "IMPOSSIBLE"
    yield $ "Case #" ++ (show $ nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
