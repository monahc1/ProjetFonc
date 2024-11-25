import Control.Monad
import Data.Time.Clock
import qualified Data.List as List
import GHC.Stats
import System.Mem 


data Chained a = Empty | Node a (Chained a) deriving (Show)


addl :: Ord a =>Chained  a -> a -> Chained a
addl Empty x = Node x Empty 
addl (Node b next) x = Node b (addl next x) 

addf :: Ord a => Chained a ->a-> Chained a
addf Empty x = Node x Empty
addf (Node m next) x = Node x (Node m next)

insert ::Chained a -> a -> Int  -> Chained a
insert Empty a _ = Node a Empty
insert (Node a next) x y 
    | y==0 = Node x (Node a next)
    | otherwise = Node a (insert next x (y-1))


remove1 :: Ord a => Chained a -> a -> Chained a
remove1 Empty _ = Empty
remove1 (Node b next) x 
    | b==x = next 
    | otherwise = Node b (remove1 next x)

makelist :: Int -> Int ->  [Int]
makelist x y = take x (iterate (+1) y)



benchmarkaddlast :: Int -> IO ()
benchmarkaddlast n = do
    let values = makelist n 0
    start <- getCurrentTime
    performGC 
    let ch1 = foldl addl Empty values
    end <- ch1 `seq` getCurrentTime
    performGC 
    stats <- getRTSStats 
    let duration = diffUTCTime end start
        memoryUsage = max_mem_in_use_bytes stats
    putStrLn $ "Time taken to addlast " ++ show n ++ " elements: " ++ show duration ++ " seconds"
    putStrLn $ "Memory used addlast: " ++ show memoryUsage ++ " bytes"

benchmarkaddfirst :: Int -> IO ()
benchmarkaddfirst n = do
    let values = makelist n 0
    start <- getCurrentTime
    performGC 
    let ch1 = foldl addf Empty values
    end <- ch1 `seq` getCurrentTime
    performGC 
    stats <- getRTSStats 
    let duration = diffUTCTime end start
        memoryUsage = max_mem_in_use_bytes stats
    putStrLn $ "Time taken to addfirst " ++ show n ++ " elements: " ++ show duration ++ " seconds"
    putStrLn $ "Memory used addfirst: " ++ show memoryUsage ++ " bytes"

benchmarkinsert :: Int -> IO ()
benchmarkinsert n = do
    let values = zip [0..] (makelist n 0) 
    start <- getCurrentTime     
    performGC            
    let ch1 = foldl (\acc (y, x) -> insert acc x (div y 2)) Empty values      
    end <- ch1 `seq` getCurrentTime 
    performGC
    stats <- getRTSStats                 
    let duration = diffUTCTime end start
        memoryUsage = max_mem_in_use_bytes stats
    putStrLn $ "Time taken to insert middle " ++ show n ++ " elements: " ++ show duration ++ " seconds"
    putStrLn $ "Memory used insert middle: " ++ show memoryUsage ++ " bytes"




main :: IO ()
main = do
    benchmarkinsert 150000
    benchmarkaddlast 150000
    benchmarkaddfirst 150000

