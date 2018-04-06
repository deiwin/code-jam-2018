module Solution where

import Pipes
import qualified Pipes.Prelude as Pipes

simple :: Int -> Int
simple x = x

solve :: Monad m => Pipe String String m ()
solve = do
    str1 <- await
    str2 <- await
    yield (str1 ++ str2)

main :: IO ()
main = runEffect $ Pipes.stdinLn >-> solve >-> Pipes.stdoutLn
