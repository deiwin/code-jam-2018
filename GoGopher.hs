type Grid = [Row]
type Row = [Bool]

emptyGrid :: Int -> Int -> Grid
emptyGrid rows columns = take rows $ repeat $ take columns $ repeat False

deployAt :: Int -> Int -> IO ()
deployAt row column = putStrLn $ (show $ row + 1) ++ " " ++ (show $ column + 1)

fillColumn :: Int -> Row -> Row
fillColumn column [] = []
fillColumn 0 (first:rest) = True:rest
fillColumn column (first:rest) = first:(fillColumn (column - 1) rest)

fill :: Int -> Int -> Grid -> Grid
fill row column [] = []
fill 0 column (firstRow:rest) = (fillColumn column firstRow):rest
fill row column (firstRow:rest) = firstRow:(fill (row - 1) column rest)

filledAt :: Int -> Int -> Grid -> Bool
filledAt row column grid = (grid !! row) !! column

solveCase' :: Int -> Int -> Int -> Int -> Grid -> IO ()
solveCase' rowsFinished columnsFinishedInRow rows columns grid
  | columnsFinishedInRow == columns = solveCase' (rowsFinished + 1) 0 rows columns grid
  | rowsFinished == rows = error "Uh oh"
  | filledAt rowsFinished columnsFinishedInRow grid = solveCase' rowsFinished (columnsFinishedInRow + 1) rows columns grid
  | otherwise =
      let rowToDeploy = min (rowsFinished + 1) (rows - 2)
          columnToDeploy = min (columnsFinishedInRow + 1) (columns - 2)
       in do
           deployAt rowToDeploy columnToDeploy
           actualLocation <- getLine
           let actualLocationList = map read $ words actualLocation
               actualRow = (actualLocationList !! 0) - 1
               actualColumn = (actualLocationList !! 1) - 1
            in if actualRow == -1 && actualColumn == -1
                  then return ()
                  else if actualRow == -2 && actualColumn == -2
                      then error "Something went wrong :("
                      else solveCase' rowsFinished columnsFinishedInRow rows columns (fill actualRow actualColumn grid)
solveCase :: Int -> IO ()
solveCase minPreparedCells =
    let rows = ceiling $ sqrt $ fromIntegral minPreparedCells
        columns = ceiling $ (fromIntegral minPreparedCells) / (fromIntegral rows)
        grid = emptyGrid rows columns
     in solveCase' 0 0 rows columns grid

solve' :: Int -> Int -> IO ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    minPreparedCells <- getLine
    solveCase $ read minPreparedCells
    solve' (testCasesLeft - 1)  nrOfTestCases

main :: IO ()
main = do
    nrOfTestCasesStr <- getLine
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases
