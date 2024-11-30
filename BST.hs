import System.Random
import Control.Monad 
import Data.Time.Clock 
import System.Mem 
import GHC.Stats 
import Control.DeepSeq 

data BST a = Empty | Node a (BST a) (BST a) deriving (Show)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node a left right)
    | x < a     = Node a (insert x left) right
    | x > a     = Node a left (insert x right)
    | otherwise = Node a left right

randomList :: Int -> IO [Int]
randomList n = replicateM n $ randomRIO (1, 1000000)

estimateMemory :: Int -> Int
estimateMemory nodes = nodes * 24  

benchmarkInsertion :: Int -> IO ()
benchmarkInsertion n = do
    values <- randomList n
    start <- getCurrentTime
    performGC
    let bst = foldr insert Empty values
    end <- bst `seq` getCurrentTime
    performGC
    stats <- getRTSStats
    let duration = diffUTCTime end start
        memoryUsage = max_mem_in_use_bytes stats
        estimatedMemory = estimateMemory n
    putStrLn $ "Benchmark for " ++ show n ++ " elements:"
    putStrLn $ "Time taken: " ++ show duration ++ " seconds"
    putStrLn $ "memory: " ++ show estimatedMemory ++ " bytes"
    putStrLn "--------------------------------------------------"

main :: IO ()
main = do
    benchmarkInsertion 100000
    benchmarkInsertion 200000
    benchmarkInsertion 500000
