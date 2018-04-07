module SavingUniverse where

import Pipes
import qualified Pipes.Prelude as Pipes

type Instructions = String

countMinDamage :: Instructions -> Int
countMinDamage = length . filter (== 'S')

solveCase :: Int -> Instructions -> Maybe Int
solveCase maxDamage instructions =
    if countMinDamage instructions > maxDamage
       then Nothing
       else Just 1

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    caseInput <- await
    let inputValues = words caseInput
        maxDamage = read $ inputValues!!0
        instructions = inputValues!!1
        solution = maybe "IMPOSSIBLE" show (solveCase maxDamage instructions)
     in yield $ "Case #" ++ (show $ nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
