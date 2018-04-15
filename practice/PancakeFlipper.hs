module PancakeFlipper where

import Pipes
import qualified Pipes.Prelude as Pipes

type IsHappy = Bool
type NumberOfFlips = Int
type FlipperLength = Int

toIsHappy :: Char -> IsHappy
toIsHappy = (== '+')

needsFlip :: IsHappy -> Bool
needsFlip = not

canFlip :: FlipperLength -> [IsHappy] -> Bool
canFlip 0 pancakes = True
canFlip flipperLength [] = False
canFlip flipperLength (first:rest) = canFlip (flipperLength - 1) rest

flipN :: Int -> [IsHappy] -> [IsHappy]
flipN 0 pancakes = pancakes
flipN n [] = error "too many flips"
flipN n (first:rest) = (not first):(flipN (n - 1) rest)

countFlips :: Int -> [IsHappy] -> FlipperLength -> Maybe NumberOfFlips
countFlips flips [] flipperLength = Just flips
countFlips flips (first:rest) flipperLength
  | canFlip flipperLength (first:rest) =
      if needsFlip first
         then countFlips (flips + 1) (flipN flipperLength (first:rest)) flipperLength
         else countFlips flips rest flipperLength
  | needsFlip first = Nothing
  | otherwise = countFlips flips rest flipperLength

solveCase :: [IsHappy] -> FlipperLength -> Maybe NumberOfFlips
solveCase = countFlips 0

solve' :: Monad m => Int -> Int -> Pipe String String m ()
solve' 0 nrOfTestCases = return ()
solve' testCasesLeft nrOfTestCases = do
    line <- await
    let pancakes = map toIsHappy $ (words line)!!0
        flipperLength = read $ (words line)!!1
        solution = maybe "IMPOSSIBLE" show (solveCase pancakes flipperLength)
     in yield $ "Case #" ++ (show $ nrOfTestCases - testCasesLeft + 1) ++ ": " ++ solution
    solve' (testCasesLeft - 1)  nrOfTestCases

solve :: Monad m => Pipe String String m ()
solve = do
    nrOfTestCasesStr <- await
    let nrOfTestCases = read nrOfTestCasesStr
     in solve' nrOfTestCases nrOfTestCases

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
