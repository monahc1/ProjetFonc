import System.Random
import Control.Monad
import Data.Time.Clock
import GHC.Stats
import System.Mem 


type Stack a = [a]

push :: a -> Stack a -> Stack a
push x s = x : s


pop :: Stack a -> (a, Stack a)
pop [] = error "Empty stack"
pop (x:xs) = (x, xs)


popAllAndSum :: Num a => Stack a -> a
popAllAndSum [] = 0
popAllAndSum s = let (x, s') = pop s in x + popAllAndSum s'

randomList :: Int -> IO [Int]
randomList n = replicateM n $ randomRIO (1, 1000000)

benchmarkStackOperations :: Int -> IO ()
benchmarkStackOperations n = do
    values <- randomList n
    performGC
    startPush <- getCurrentTime
    let stack = foldr push [] values
    endPush <- stack seq getCurrentTime  -- Force evaluation of the push operation
    performGC
    statsPush <- getRTSStats
    startPop <- getCurrentTime
    let result = popAllAndSum stack
    endPop <- result seq getCurrentTime  -- Force evaluation of the pop operation
    performGC
    statsPop <- getRTSStats
    let pushDuration = diffUTCTime endPush startPush
        popDuration = diffUTCTime endPop startPop
        memoryUsagePush = max_mem_in_use_bytes statsPush
        memoryUsagePop = max_mem_in_use_bytes statsPop
    putStrLn $ "Operation: Push " ++ show n ++ " elements"
    putStrLn $ "Time: " ++ show pushDuration ++ " seconds"
    putStrLn $ "Memory: " ++ show memoryUsagePush ++ " bytes"
    putStrLn $ "Operation: Pop " ++ show n ++ " elements"
    putStrLn $ "Time: " ++ show popDuration ++ " seconds"
    putStrLn $ "Memory: " ++ show memoryUsagePop ++ " bytes"


main :: IO ()
main = do
    benchmarkStackOperations 100000
    benchmarkStackOperations 200000
    benchmarkStackOperations 500000
