import Control.DeepSeq (NFData, rnf, deepseq)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Mem (performGC)
import GHC.Stats (getRTSStats, max_mem_in_use_bytes)

data Queue a = Queue [a] [a] deriving (Show)

instance NFData a => NFData (Queue a) where
    rnf (Queue front rear) = rnf front seq rnf rear

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front rear) = Queue front (x:rear)

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] []) = error "Empty queue"
dequeue (Queue [] rear) = dequeue (Queue (reverse rear) [])
dequeue (Queue (x:xs) rear) = (x, Queue xs rear)

randomList :: Int -> IO [Int]
randomList n = replicateM n $ randomRIO (1, 1000000)

estimateMemory :: Int -> Int
estimateMemory elements = elements * 8  

benchmarkQueueOperations :: Int -> IO ()
benchmarkQueueOperations n = do
    values <- randomList n
    start <- getCurrentTime
    performGC
    let queue = foldl (flip enqueue) emptyQueue values
    queue deepseq return ()
    end <- getCurrentTime
    performGC
    stats <- getRTSStats
    let duration = diffUTCTime end start
        memoryUsage = max_mem_in_use_bytes stats
        estimatedMemory = estimateMemory n
    putStrLn $ "Benchmark for " ++ show n ++ " elements:"
    putStrLn $ "Time taken: " ++ show duration ++ " seconds"
    putStrLn $ "Memory: " ++ show memoryUsage ++ " bytes"
    putStrLn $ "Estimated memory usage: " ++ show estimatedMemory ++ " bytes"
    putStrLn "--------------------------------------------------"

main :: IO ()
main = do
    benchmarkQueueOperations 100000
    benchmarkQueueOperations 200000
    benchmarkQueueOperations 500000