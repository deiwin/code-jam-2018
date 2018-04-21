module BitParty where

import Pipes
import qualified Pipes.Prelude as Pipes
import Control.Monad (foldM)
import Data.List (sortBy, insertBy)
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

putSortedByLeastTimeForBit :: Cashier -> [Cashier] -> [Cashier]
putSortedByLeastTimeForBit = insertBy $ comparing timeWithNextBit

sortCashiersByLeastTimeForBit :: [Cashier] -> [Cashier]
sortCashiersByLeastTimeForBit = sortBy $ comparing timeWithNextBit

noRobotAssigned :: Cashier -> Bool
noRobotAssigned cashier = bitCapacity cashier == maxBits cashier

totalTime :: [Cashier] -> [Cashier] -> [Cashier] -> Int -> Int -> Int
-- Robot counts shouldn't fall below 0
totalTime cashiers filledCashiers ignoredCashiers robotCount bitCount
  | robotCount < 0 = error "uh oh"

-- If we run out of cashiers to use (all are eiteher filled or ignored), then
-- remove the first (the one with the smallest maxBits, because that will give
-- us the least amount of extra work to do) cashier from filledCashiers and use
-- the first one from ignored cashiers instead.
totalTime [] (firstFilled:restFilled) (firstIgnored:restIgnored) 0 bitCount =
    totalTime [firstIgnored] restFilled restIgnored 1 (bitCount + maxBits firstFilled)

-- Cashiers should only run empty in the above case. E.g. the robot count
-- should be 0.
totalTime [] _ _ _ _ = error "Shouldn't happen"

-- If we don't have any more robots, and the first cashier doesn't have a robot
-- yet (maxBits == bitCapacity), then ignore the cashier for now
totalTime (first:rest) filledCashiers ignoredCashiers 0 bitCount
  | noRobotAssigned first =
      totalTime rest filledCashiers (putSortedByLeastTimeForBit first ignoredCashiers) 0 bitCount

-- We have one last bit to give and the first cashier is ready to receive it
-- and is the best candidate for it. The answer is however long it takes for
-- this cashier to process all current bits and the one last bit, because all
-- other cashiers will be done faster.
totalTime (first:rest) _ _ robotCount 1 = timeWithNextBit first

-- Otherwise give one bit to the first cashier in the list (the fastest one to
-- process one additional bit). If that's the last bit the cashier can process,
-- then put them into the filledCashiers list, otherwise put them back into the
-- list of cashiers, keeping the list sorted.
totalTime (first:rest) filledCashiers ignoredCashiers robotCount bitCount =
    let updatedCashier = first { timeWithNextBit=timeWithNextBit first + scanTime first
                               , bitCapacity=bitCapacity first - 1
                               }
        nextCashiers = if bitCapacity updatedCashier == 0
                          then rest
                          else putSortedByLeastTimeForBit updatedCashier rest
        nextFilledCashiers = if bitCapacity updatedCashier == 0
                                then insertBy (comparing maxBits) updatedCashier filledCashiers
                                else filledCashiers
        nextRobotCount = if noRobotAssigned first
                            then robotCount - 1
                            else robotCount
     in totalTime nextCashiers nextFilledCashiers ignoredCashiers nextRobotCount (bitCount - 1)

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (robotCount:bitCount:cashierCount:_) = map read $ words line
    cashiers <- readCashiers cashierCount
    let sortedCashiers = sortBy
    let solution = show $ totalTime cashiers [] [] robotCount bitCount
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn