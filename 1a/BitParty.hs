module BitParty where

import Pipes
import qualified Pipes.Prelude as Pipes
import Control.Monad
import Data.List
import Data.Ord

data Cashier = Cashier { maxBits :: Integer
                       , scanTime :: Integer
                       , packingTime :: Integer
                       } deriving (Show, Eq)

readCashiers :: Monad m => Int -> Pipe String String m [Cashier]
readCashiers rowsToRead =
    foldM fn [] [1..rowsToRead]
        where fn acc id = do line <- await
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

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (robotCount:bitCount:cashierCount:_) = map read $ words line
    cashiers <- readCashiers cashierCount
    let solution = show $ binarySearch (\x -> canSolve x robotCount (fromIntegral bitCount) cashiers) 0 (10^9)
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
