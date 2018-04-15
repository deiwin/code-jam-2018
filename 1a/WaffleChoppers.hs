module WaffleChoppers where

import Pipes
import qualified Pipes.Prelude as Pipes
import Control.Monad
import Data.Bool

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

chipsInColumns' :: [Int] -> Row -> [Int]
chipsInColumns' acc = zipWith (+) acc . (map $ bool 0 1)
chipsInColumns :: Int -> Grid -> [Int]
chipsInColumns columns grid = foldl chipsInColumns' (take columns $ repeat 0) grid

chipsInRows :: Grid -> [Int]
chipsInRows = map (foldl (+) 0 . (map $ bool 0 1))

totalChips :: Grid -> Int
totalChips = foldl (+) 0 . chipsInRows

canDistribute' :: Int -> [Int] -> Int -> Bool
canDistribute' chipsLeftPerSlice [] chipsPerSlice = chipsLeftPerSlice == chipsPerSlice
canDistribute' chipsLeftPerSlice (chipsInUnit:rest) chipsPerSlice
  | chipsInUnit > chipsLeftPerSlice = False
  | chipsInUnit == chipsLeftPerSlice = canDistribute' chipsPerSlice rest chipsPerSlice
  | otherwise = canDistribute' (chipsLeftPerSlice - chipsInUnit) rest chipsPerSlice
canDistribute :: [Int] -> Int -> Bool
canDistribute chipsToDistribute chipsPerSlice =
    canDistribute' chipsPerSlice chipsToDistribute chipsPerSlice

canCut :: Grid -> Int -> Int -> Int -> Int -> Bool
canCut grid rows columns hCuts vCuts =
    let inColumns = chipsInColumns columns grid
        inRows = chipsInRows grid
        total = foldl (+) 0 inRows
        hSlices = hCuts + 1
        vSlices = vCuts + 1
        (chipsPerHSlice, hLeftOver) = divMod total hSlices
        (chipsPerVSlice, vLeftOver) = divMod total vSlices
     in if hLeftOver /= 0 || vLeftOver /= 0
           then False
           else canDistribute inRows chipsPerHSlice &&
               canDistribute inColumns chipsPerVSlice

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (rows:columns:hCuts:vCuts:_) = map read $ words line
    grid <- readGrid rows
    let solution = bool "IMPOSSIBLE" "POSSIBLE" $ canCut grid rows columns hCuts vCuts
    yield $ "Case #" ++ (show $ nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
