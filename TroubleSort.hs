module TroubleSort where

import Pipes
import qualified Pipes.Prelude as Pipes

type ErrorIndex = Int

solveCase :: Int -> [Int] -> Maybe ErrorIndex
solveCase 5 list = Nothing
solveCase length list = Just 0

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
