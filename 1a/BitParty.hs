module BitParty where

import Pipes
import qualified Pipes.Prelude as Pipes
import Control.Monad (foldM)
import Data.List (sortBy)
import Data.Ord (comparing)

-- TODO use Integer where needed
data Cashier = Cashier { timeWithNextBit :: Int
                       , bitCapacity :: Int
                       , maxBits :: Int
                       , scanTime :: Int
                       } deriving (Show, Eq)

readCashiers :: Monad m => Int -> Pipe String String m [Cashier]
readCashiers rowsToRead =
    foldM fn [] [1..rowsToRead]
        where fn acc id = do line <- await
                             let (maxBits:scanTime:packingTime:_) = map read $ words line
                                 timeWithNextBit = fromIntegral packingTime + scanTime
                              in return $ Cashier { timeWithNextBit=timeWithNextBit
                                                  , bitCapacity=maxBits
                                                  , maxBits=maxBits
                                                  , scanTime=scanTime
                                                  }:acc

putSortedBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
putSortedBy _ val [] = [val]
putSortedBy pred val (first:rest) =
    let ord = pred val first
     in case ord of GT -> first:putSortedBy pred val rest
                    _ -> val:first:rest

putSortedByLeastTimeForBit :: Cashier -> [Cashier] -> [Cashier]
putSortedByLeastTimeForBit = putSortedBy $ comparing timeWithNextBit

sortCashiersByLeastTimeForBit :: [Cashier] -> [Cashier]
sortCashiersByLeastTimeForBit = sortBy $ comparing timeWithNextBit

totalTime :: [Cashier] -> [Cashier] -> Int -> Int -> Int
totalTime cashiers (first:rest) 0 bitCount =
    totalTime cashiers rest 1 (bitCount + maxBits first)
totalTime (first:rest) _ robotCount 1 = timeWithNextBit first
totalTime (first:rest) filledCashiers robotCount bitCount =
    let updatedCashier = first { timeWithNextBit=timeWithNextBit first + scanTime first
                               , bitCapacity=bitCapacity first - 1
                               }
        nextCashiers = if bitCapacity updatedCashier == 0
                          then rest
                          else putSortedByLeastTimeForBit updatedCashier rest
        nextFilledCashiers = if bitCapacity updatedCashier == 0
                                then putSortedBy (comparing maxBits) updatedCashier filledCashiers
                                else filledCashiers
        nextRobotCount = if maxBits first == bitCapacity first
                            then robotCount - 1
                            else robotCount
     in totalTime nextCashiers nextFilledCashiers nextRobotCount (bitCount - 1)

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (robotCount:bitCount:cashierCount:_) = map read $ words line
    cashiers <- readCashiers cashierCount
    let sortedCashiers = sortBy
    let solution = show $ totalTime cashiers [] robotCount bitCount
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
