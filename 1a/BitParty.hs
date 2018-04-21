import Control.Monad
import Data.List
import Data.Ord

data Cashier = Cashier { maxBits :: Integer
                       , scanTime :: Integer
                       , packingTime :: Integer
                       } deriving (Show, Eq)

readCashiers :: Int -> IO [Cashier]
readCashiers rowsToRead =
    foldM fn [] [1..rowsToRead]
        where fn acc id = do line <- getLine
                             let (maxBits:scanTime:packingTime:_) = map read $ words line
                                 timeWithNextBit = fromIntegral packingTime + scanTime
                              in return $ Cashier { maxBits=maxBits
                                                  , scanTime=scanTime
                                                  , packingTime=packingTime
                                                  }:acc

capacity :: Integer -> Cashier -> Integer
capacity maxTime cashier = max 0 $ min (maxBits cashier) $
    (maxTime - packingTime cashier) `quot` scanTime cashier

canSolve :: Integer -> Int -> Integer -> [Cashier] -> Bool
canSolve maxTime robotCount bits =
    (>= bits) .
    sum .
    take robotCount .
    sortBy (flip compare) .
    map (capacity maxTime)

binarySearch :: (Integer -> Bool) -> Integer -> Integer -> Integer
binarySearch pred start end
  | start == end = start
  | otherwise =
      let midway = start + ((end - start) `quot` 2)
       in if pred midway
             then binarySearch pred start midway
             else binarySearch pred (midway + 1) end

solve' :: Int -> Int -> IO ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- getLine
    let (robotCount:bitCount:cashierCount:_) = map read $ words line
    cashiers <- readCashiers cashierCount
    let solution = show $ binarySearch (\x -> canSolve x robotCount (fromIntegral bitCount) cashiers) 0 (10^9)
    putStrLn $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

main :: IO ()
main = do
    nrOfTestCasesStr <- getLine
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases
