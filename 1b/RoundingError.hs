module RoundingError where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.List
import Data.Ord

sortDesc = sortBy (flip compare)

getAdditionalPoints :: Int -> Int -> Int -> Int -> Int -> [Int] -> (Int, Int)
getAdditionalPoints pointAcc 0 perPerson bucketSize goal _ = (pointAcc, 0)
getAdditionalPoints pointAcc remainingPeople perPerson bucketSize goal [] = (pointAcc, remainingPeople)
getAdditionalPoints pointAcc remainingPeople perPerson bucketSize goal (first:rest)
  | first >= bucketSize =
      let (addP, rem) = quotRem first bucketSize
       in getAdditionalPoints (pointAcc + addP) remainingPeople perPerson bucketSize goal (insertBy (flip compare) rem rest)
  | first >= goal = getAdditionalPoints (pointAcc + 1) remainingPeople perPerson bucketSize goal rest
  | otherwise =
      if perPerson >= goal
         then (pointAcc, remainingPeople)
         else let peopleRequired = quotUp (goal - first) perPerson
               in if peopleRequired <= remainingPeople
                     then getAdditionalPoints (pointAcc + 1) (remainingPeople - peopleRequired) perPerson bucketSize goal rest
                     else (pointAcc, remainingPeople)

quotUp :: Int -> Int -> Int
quotUp a b =
    let (res, rem) = quotRem a b
     in if rem == 0 then res else res + 1

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let (peopleCount:languageCount:_) = map read $ words line
    let (perPersonBaseline, perPersonNumerator) = quotRem 100 peopleCount
    let baseline = peopleCount * perPersonBaseline
    line2 <- await
    let currentVotes = map read $ words line2
    let votedPeople = sum currentVotes
    let initialRemainingPeople = peopleCount - votedPeople
    let goalPerBucket = quotUp peopleCount 2
    let sortedBuckets = sortDesc $ map (* perPersonNumerator) currentVotes
    let (additionalPoints, remainingPeople) = getAdditionalPoints 0 initialRemainingPeople perPersonNumerator peopleCount goalPerBucket sortedBuckets
    let peoplePerGoal = quotUp goalPerBucket perPersonNumerator
    let addFromRemPeople = quot remainingPeople peoplePerGoal
    let solution = if perPersonNumerator == 0
        then baseline
        else baseline + additionalPoints + addFromRemPeople
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ show solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
