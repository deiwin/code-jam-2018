module AlphabetCake where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.Array.IArray
import Control.Monad
import Data.Foldable
import Data.Maybe
import Control.Applicative

type MBounds = ((Int, Int), (Int, Int))
type MIx = (Int, Int)

type Cell = Maybe Char
type Table = Array MIx Cell
type SolvedTable = Array MIx Char

parseTable :: Int -> Int -> [String] -> Table
parseTable rows columns strings = accumArray (<|>) initial bounds assocList
        where initial = Nothing
              bounds = ((1, 1), (rows, columns))
              assocList = do
                  (i, string) <- zip [1..] strings
                  (j, char) <- zip [1..] string
                  guard $ char /= '?'
                  return ((i, j), Just char)

readTable :: Monad m => Int -> Int -> Pipe String String m Table
readTable rows columns = parseTable rows columns <$> replicateM rows await

getLetterIxs :: MBounds -> Table -> [MIx]
getLetterIxs subBounds table = do
    ix <- range subBounds
    guard $ isJust $ table!ix
    return ix

cutH :: MBounds -> Int -> (MBounds, MBounds)
cutH ((rowStart, colStart), (rowEnd, colEnd)) atRow =
    ( ((rowStart, colStart), (atRow, colEnd))
    , ((atRow + 1, colStart), (rowEnd, colEnd))
    )

cutV :: MBounds -> Int -> (MBounds, MBounds)
cutV ((rowStart, colStart), (rowEnd, colEnd)) atCol =
    ( ((rowStart, colStart), (rowEnd, atCol))
    , ((rowStart, atCol + 1), (rowEnd, colEnd))
    )

cut :: [MIx] -> MBounds -> (MBounds, MBounds)
cut letterIxs subBounds =
    let ((row1, col1):(row2, col2):_) = take 2 letterIxs
     in if col1 == col2
           then cutH subBounds (min row1 row2)
           else cutV subBounds (min col1 col2)

fillWith :: Maybe Char -> MBounds -> Table -> Table
fillWith mChar subBounds table = table // do
    ix <- range subBounds
    return (ix, mChar)

mergeTables :: MBounds -> Table -> Table -> Table
mergeTables subBounds tableA tableB = tableA // do
    ix <- range subBounds
    return (ix, tableB!ix)

solveTable' :: MBounds -> Table -> Table
solveTable' subBounds table =
    let letterIxs = getLetterIxs subBounds table
     in case length letterIxs of
          0 -> error "shouldn't happen"
          1 -> fillWith (table ! head letterIxs) subBounds table
          _ -> let (boundsA, boundsB) = cut letterIxs subBounds
                in mergeTables boundsB (solveTable' boundsA table) (solveTable' boundsB table)
solveTable :: Table -> SolvedTable
solveTable table = amap fromJust $ solveTable' (bounds table) table

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto columns list = let (head, rest) = splitAt columns list
                  in head:splitInto columns rest

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (rows:columns:_) = map read $ words line
    table <- readTable rows columns
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ":"
    let solution = splitInto columns $ toList $ solveTable table
    traverse_ yield solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
