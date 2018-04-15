module WaffleChoppers where

import Pipes
import qualified Pipes.Prelude as Pipes
import Control.Monad

type HasChip = Bool
type Row = [HasChip]
type Grid = [Row]

hasChip :: Char -> HasChip
hasChip = (== '@')

readGrid :: Monad m => Int -> Pipe String String m Grid
readGrid rowsToRead =
    foldM fn [] [1..rowsToRead]
        where fn acc _ = do line <- await
                            return ((map hasChip line):acc)

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
