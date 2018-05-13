module RoadSigns where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.List
import Data.Either
import Data.Maybe
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S

data Sign = Sign { fromSignfield :: Int
                 , eastToM :: Int
                 , westToN :: Int
                 } deriving (Show, Eq)

-- Left = locked - has to be that value
-- Right = still open - has to be a value in the set
type PossibleTown = Either Int (Set Int)
type PossibleCombination = (PossibleTown, PossibleTown)
-- Max contiguous length, and count
type Acc = (Int, Int)

possibleM :: Sign -> Int
possibleM sign = fromSignfield sign + eastToM sign

possibleN :: Sign -> Int
possibleN sign = fromSignfield sign - westToN sign

matches :: PossibleTown -> Int -> Bool
matches (Left a) b = a == b
matches (Right set) b = S.member b set

add :: PossibleTown -> Int -> PossibleTown
add (Left a) b = Left a
add (Right set) b = Right $ S.insert b set

singletonCombination :: Sign -> [PossibleCombination]
singletonCombination sign = [(Right (S.singleton (possibleM sign)), Right (S.singleton (possibleN sign)))]

combinePossibilities :: Sign -> PossibleCombination -> [PossibleCombination]
combinePossibilities sign (m, n)
  | matches m (possibleM sign) && matches n (possibleN sign) = [(m, n)]
  | matches m (possibleM sign) = [(Left (possibleM sign), add n (possibleN sign))]
  | matches n (possibleN sign) = [(add m (possibleM sign), Left (possibleN sign))]
  | (Right mSet) <- m, (Right nSet) <- n =
      map (\x -> (Left (possibleM sign), Left x)) (S.toList nSet) ++
          map (\x -> (Left x, Left (possibleN sign))) (S.toList mSet)
  | (Right _) <- m = [(Left (possibleM sign), n)]
  | (Right _) <- n = [(m, Left (possibleN sign))]
  | otherwise = []

addMax :: Int -> Acc -> Acc
addMax l (maxL, count)
  | l > maxL = (l, 1)
  | l == maxL = (maxL, count + 1)
  | otherwise = (maxL, count)

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

checkSign :: ([[PossibleCombination]], Acc) -> Sign -> ([[PossibleCombination]], Acc)
checkSign (possibleCombinations, acc) sign =
    let remainingCombinations = takeWhile (not . null) $ (>>= combinePossibilities sign) <$> possibleCombinations
        newCombinations = singletonCombination sign:remainingCombinations
        curLength = length newCombinations
        newAcc = addMax curLength acc
     in (newCombinations, newAcc)

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    roadSignCount <- read <$> await
    signs <- map ((\(d:a:b:_) -> Sign d a b) . map read . words) <$> replicateM roadSignCount await
    let solution = unwords $ map show $ tupleToList $ snd $ foldl' checkSign ([], (0, 0)) signs
    yield $ "Case #" ++ show (nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
