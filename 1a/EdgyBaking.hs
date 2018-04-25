import Control.Monad
import Control.Arrow ((***))
import Numeric
import Data.List

type Range = (Double, Double)
type Cookie = (Int, Int)

readCookies :: Int -> IO [Cookie]
readCookies rowsToRead =
    foldM fn [] [1..rowsToRead]
        where fn acc id = do line <- getLine
                             let (width:height:_) = map read $ words line
                              in return $ (width, height):acc

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

addedRange :: Cookie -> Range
addedRange (width, height) =
    let lowBound = fromIntegral (min width height)
        highBound = sqrt $ fromIntegral width^2 + fromIntegral height^2
     in mapTuple (* 2) (lowBound, highBound)

add :: Range -> Range -> Range
add (minA, maxA) (minB, maxB) = (minA + minB, maxA + maxB)

within :: Range -> Int -> Bool
within (low, high) point = low <= point' && point' <= high
    where point' = fromIntegral point

below :: Range -> Int -> Bool
below (low, _) point = point' < low
    where point' = fromIntegral point

perimeter :: Cookie -> Int
perimeter (width, height) = 2 * (width + height)

overlap :: Range -> Range -> Bool
overlap (minA, maxA) (minB, maxB) = maxB >= minA && minB <= maxA

merge :: Range -> Range -> Range
merge (minA, maxA) (minB, maxB) = (min minA minB, max maxA maxB)

mergeAll' :: Range -> [Range] -> [Range]
mergeAll' range [] = [range]
mergeAll' range (first:rest)
  | overlap range first = merge range first:rest
  | otherwise = range:first:rest
mergeAll :: [Range] -> [Range]
mergeAll = foldr mergeAll' []

addToAll' :: Range -> Range -> [Range] -> [Range]
addToAll' rangeToAdd currentRange rest = currentRange:insert (add rangeToAdd currentRange) rest
addToAll :: Range -> [Range] -> [Range]
addToAll range = foldr (addToAll' range) []

addAndMergeAll :: [Range] -> Range -> [Range]
addAndMergeAll acc range = mergeAll $ insert range $ addToAll range acc

rangesForCookies :: [Cookie] -> [Range]
rangesForCookies = foldl' addAndMergeAll [] . map addedRange

findMaxUnder' :: Double -> Int -> [Range] -> Double
findMaxUnder' mem target [] = mem
findMaxUnder' mem target (first:rest)
  | within first target = fromIntegral target
  | below first target = mem
  | otherwise = findMaxUnder' (snd first) target rest
findMaxUnder :: Int -> [Range] -> Double
findMaxUnder = findMaxUnder' 0

solveCase :: [Cookie] -> Int -> Double
solveCase cookies goal =
    let initialPerimeter = sum $ map perimeter cookies
        maximzationTarget = goal - initialPerimeter
        maximized = findMaxUnder maximzationTarget $ rangesForCookies cookies
     in fromIntegral initialPerimeter + maximized

solve' :: Int -> Int -> IO ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- getLine
    let (cookieCount:goal:_) = map read $ words line
    cookies <- readCookies cookieCount
    let solution = showFFloat (Just 6) (solveCase cookies goal) ""
    putStrLn $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

main :: IO ()
main = do
    nrOfTestCasesStr <- getLine
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases
