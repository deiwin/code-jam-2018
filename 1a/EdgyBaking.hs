import Control.Monad
import Control.Arrow ((***))
import Numeric

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

-- Todo if above range
cutUntilWithinRange :: [Cookie] -> Range -> Int -> Double
cutUntilWithinRange [] range goal = snd range
cutUntilWithinRange (first:rest) range goal =
    let newRange = add range (addedRange first)
     in if within newRange goal
           then fromIntegral goal
           else if below newRange goal
           then snd range
           else cutUntilWithinRange rest newRange goal

solve' :: Int -> Int -> IO ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- getLine
    let (cookieCount:goal:_) = map read $ words line
    cookies <- readCookies cookieCount
    let initialPerimeter = fromIntegral $ sum $ map perimeter cookies
    let initialRange = (initialPerimeter, initialPerimeter)
    let solution = showFFloat (Just 6) (cutUntilWithinRange cookies initialRange goal) ""
    putStrLn $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

main :: IO ()
main = do
    nrOfTestCasesStr <- getLine
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases
