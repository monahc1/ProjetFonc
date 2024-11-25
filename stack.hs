import Control.Monad
import Data.Time.Clock
import System.Random (randomRs, newStdGen)


type Stack a = [a]


push :: a -> Stack a -> Stack a
push x s = x : s


pop :: Stack a -> (a, Stack a)
pop [] = error "Empty stack"
pop (x:xs) = (x, xs)


peek :: Stack a -> a
peek [] = error "Empty stack"
peek (x:_) = x


isEmpty :: Stack a -> Bool
isEmpty [] = True
isEmpty _  = False


popAll :: Stack Int -> Stack Int
popAll [] = []
popAll s = let (_, s') = pop s in popAll s'


randomList :: Int -> IO [Int]
randomList n = do
    gen <- newStdGen
    return $ take n $ randomRs (0, n) gen


benchmarkStackOperations :: Int -> IO ()
benchmarkStackOperations n = do
    values <- randomList n
    start <- getCurrentTime
    let stack = foldr push [] values
    mid <- getCurrentTime
    let _ = popAll stack
    end <- getCurrentTime
    let pushDuration = diffUTCTime mid start
    let popDuration = diffUTCTime end mid
    putStrLn $ "Time taken to push " ++ show n ++ " elements: " ++ show pushDuration ++ " seconds"
    putStrLn $ "Time taken to pop " ++ show n ++ " elements: " ++ show popDuration ++ " seconds"


main :: IO ()
main = do
    benchmarkStackOperations 100000
    benchmarkStackOperations 200000
    benchmarkStackOperations 500000
