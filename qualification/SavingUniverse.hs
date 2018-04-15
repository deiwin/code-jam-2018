module SavingUniverse where

import Pipes
import qualified Pipes.Prelude as Pipes

type Instructions = String

countMinDamage :: Instructions -> Int
countMinDamage = length . filter (== 'S')

countCurrentDamage' :: (Int, Int) -> Char -> (Int, Int)
countCurrentDamage' (damage, charge) 'C' = (damage, charge * 2)
countCurrentDamage' (damage, charge) 'S' = (damage + charge, charge)
countCurrentDamage :: Instructions -> Int
countCurrentDamage = fst . foldl countCurrentDamage' (0, 1)

hack' :: Char -> (Instructions, Bool) -> (Instructions, Bool)
hack' instruction (instructions, True) = (instruction:instructions, True)
hack' instruction ([], False) = ([instruction], False)
hack' instruction (first:rest, False)
  | instruction == 'C' && first == 'S' = (first:instruction:rest, True)
  | otherwise = (instruction:first:rest, False)
hack :: Instructions -> Instructions
hack = fst . foldr hack' ([], False)

countMinHacks' :: Int -> Instructions -> Int -> Int
countMinHacks' nthHack instructions maxDamage
  | countCurrentDamage instructions <= maxDamage = nthHack
  | otherwise = countMinHacks' (nthHack + 1) (hack instructions) maxDamage
countMinHacks :: Instructions -> Int -> Int
countMinHacks = countMinHacks' 0

solveCase :: Int -> Instructions -> Maybe Int
solveCase maxDamage instructions
  | countMinDamage instructions > maxDamage = Nothing
  | otherwise = Just $ countMinHacks instructions maxDamage

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
