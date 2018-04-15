module TroubleSort where

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.List (sort)
type ErrorIndex = Int

split :: [Int] -> [(Int, Int)]
split (first:second:rest) = (first, second):(split rest)
split (first:rest) = (first, maxBound):(split rest)
split [] = []

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

getErrorIndex' :: Int -> ([Int], [Int]) -> Maybe ErrorIndex
getErrorIndex' i ((first:third:rest1), (second:fourth:rest2))
  | second < first = Just i
  | third < second = Just $ i + 1
  | fourth < third = Just $ i + 2
  | otherwise = getErrorIndex' (i + 2) ((third:rest1), (fourth:rest2))
getErrorIndex' i ((first:rest1), (second:rest2))
  | second < first = Just i
  | otherwise = getErrorIndex' (i + 2) (rest1, rest2)
getErrorIndex' _ _ = Nothing

getErrorIndex :: ([Int], [Int]) -> Maybe ErrorIndex
getErrorIndex = getErrorIndex' 0

solveCase :: Int -> [Int] -> Maybe ErrorIndex
solveCase _ = getErrorIndex . (mapTuple sort) . unzip . split

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    length <- await
    list <- await
    let solution = maybe "OK" show (solveCase (read length) (map read (words list)))
     in yield $ "Case #" ++ (show $ nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
