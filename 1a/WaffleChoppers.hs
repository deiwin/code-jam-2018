import Control.Monad
import Control.Applicative
import Data.Bool

type ChipCount = Int
type Row = [ChipCount]
type Grid = [Row]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

hasChip :: Char -> Bool
hasChip = (== '@')

readGrid :: Int -> IO Grid
readGrid rowsToRead =
    foldM fn [] [1..rowsToRead]
        where fn acc _ = do line <- getLine
                            return (map (bool 0 1 . hasChip) line:acc)

chipsInColumns :: Int -> Grid -> [Int]
chipsInColumns columns = foldl (zipWith (+)) $ replicate columns 0

chipsInRows :: Grid -> [Int]
chipsInRows = map sum

totalChips :: Grid -> Int
totalChips = sum . chipsInRows

canDistribute :: Int -> Grid -> Bool
canDistribute chipsPerPiece = all $ all (== chipsPerPiece)

reduceRowToPieces' :: Int -> (Row, Int, Int) -> (Int, Int) -> Maybe (Row, Int, Int)
reduceRowToPieces' perVSlice (row, chipsLeftPerSlice, chipAcc) (chipsInColumn, chipsInSquare)
  | chipsInColumn > chipsLeftPerSlice = Nothing
  | chipsInColumn == chipsLeftPerSlice =
      let newRow = (chipsInSquare + chipAcc):row
       in Just (newRow, perVSlice, 0)
  | otherwise =
      let newChipsLeftPerSlice = chipsLeftPerSlice - chipsInColumn
          newChipAcc = chipAcc + chipsInSquare
       in Just (row, newChipsLeftPerSlice, newChipAcc)
reduceRowToPieces :: [Int] -> Int -> Row -> Maybe Row
reduceRowToPieces inColumns perVSlice =
    let fn = reduceRowToPieces' perVSlice
        initialChipAcc = 0
     in fmap fst3 .
         foldM fn ([], perVSlice, initialChipAcc) .
             zip inColumns

reduceGridToPieces' :: Int -> (Grid, Int, Row) -> (Int, Maybe Row) -> Maybe (Grid, Int, Row)
reduceGridToPieces' perHSlice acc (chipsInRow, Nothing) = Nothing
reduceGridToPieces' perHSlice (grid, chipsLeftPerSlice, accRow) (chipsInRow, Just row)
  | chipsInRow > chipsLeftPerSlice = Nothing
  | chipsInRow == chipsLeftPerSlice =
      let newGrid = zipWith (+) row accRow:grid
          emptyAccRow = map (const 0) accRow
       in Just (newGrid, perHSlice, emptyAccRow)
  | otherwise =
      let newChipsLeftPerSlice = chipsLeftPerSlice - chipsInRow
          newAccRow = zipWith (+) row accRow
       in Just (grid, newChipsLeftPerSlice, newAccRow)
reduceGridToPieces :: [Int] -> Int -> [Int] -> Int -> Grid -> Maybe Grid
reduceGridToPieces inRows perHSlice inColumns perVSlice =
    let initialAccRow = map (const 0) inColumns
        fn = reduceGridToPieces' perHSlice
     in fmap fst3 .
         foldM fn ([], perHSlice, initialAccRow) .
             zip inRows .
                 map (reduceRowToPieces inColumns perVSlice)

canCut :: Grid -> Int -> Int -> Int -> Int -> Bool
canCut grid rows columns hCuts vCuts =
    let inColumns = chipsInColumns columns grid
        inRows = chipsInRows grid
        total = sum inRows
        hSlices = hCuts + 1
        vSlices = vCuts + 1
        (chipsPerHSlice, hLeftOver) = quotRem total hSlices
        (chipsPerVSlice, vLeftOver) = quotRem total vSlices
        (chipsPerPiece, totalLeftOver) = quotRem total (hSlices * vSlices)
     in hLeftOver == 0 && vLeftOver == 0 && totalLeftOver == 0 &&
         maybe False (canDistribute chipsPerPiece)
             (reduceGridToPieces inRows chipsPerHSlice inColumns chipsPerVSlice grid)

solve' :: Int -> Int -> IO ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- getLine
    let (rows:columns:hCuts:vCuts:_) = map read $ words line
    grid <- readGrid rows
    let solution = bool "IMPOSSIBLE" "POSSIBLE" $ canCut grid rows columns hCuts vCuts
    putStrLn $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

main :: IO ()
main = do
    nrOfTestCasesStr <- getLine
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases
