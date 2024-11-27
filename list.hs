import Control.Monad
import Data.Time.Clock
import qualified Data.List as List
import GHC.Stats 
import System.Mem 
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Control.DeepSeq (deepseq)
import Distribution.Simple.Bench (bench) 
data Array1 s a = Array
  { arr    :: STArray s Int a  
  , size   :: Int             
  , posRef :: STRef s Int      
  }

newArray1 :: Int -> ST s (Array1 s a)
newArray1 n = do
  stArray <- newArray_ (0, n - 1)  
  pos <- newSTRef 0               
  return $ Array stArray n pos

addLast :: Array1 s a -> a -> ST s ()
addLast array value = do
  pos <- readSTRef (posRef array)
  if pos >= size array
    then error "Array is full"
    else do
      writeArray (arr array) pos value
      writeSTRef (posRef array) (pos + 1)

addFirst :: Array1 s a -> a -> ST s ()
addFirst array value = do
  pos <- readSTRef (posRef array)
  if pos >= size array
    then error "Array is full"
    else do
      forM_ [pos - 1, pos - 2 .. 0] $ \i -> do
        elem <- readArray (arr array) i
        writeArray (arr array) (i + 1) elem
      writeArray (arr array) 0 value
      writeSTRef (posRef array) (pos + 1)

insertAt :: Array1 s a -> Int -> a -> ST s ()
insertAt array index value = do
  pos <- readSTRef (posRef array)
  if index < 0 || index >= size array
    then error "Index out of bounds"
    else if pos >= size array
      then error "Array is full"
      else if index >= pos
        then do
          writeArray (arr array) pos value
          writeSTRef (posRef array) (pos + 1)
        else do
          forM_ [pos - 1, pos - 2 .. index] $ \i -> do
            elem <- readArray (arr array) i
            writeArray (arr array) (i + 1) elem
          writeArray (arr array) index value
          writeSTRef (posRef array) (pos + 1)

showArray :: Show a => Array1 s a -> ST s [a]
showArray array = do
  pos <- readSTRef (posRef array)
  forM [0 .. pos - 1] $ \i -> readArray (arr array) i

makelist :: Int -> Int ->  [Int]
makelist x y = take x (iterate (+1) y)

benchmarkaddlast :: Int -> IO ()
benchmarkaddlast n = do
  let values = makelist n 0
  start <- getCurrentTime
  let finalArray = runST $ do
        array <- newArray1 n
        forM_ values (addLast array)  
        showArray array  
  finalArray `deepseq` return ()
  end <- getCurrentTime
  let arrayMemory = n * 8  
  let stRefMemory = 16     
  let totalMemory = arrayMemory + stRefMemory + 24  
      
  let duration = diffUTCTime end start 
  putStrLn $ "memory: " ++ show totalMemory ++ " bytes" 
  putStrLn $ "Time taken to addLast " ++ show n ++ " elements: " ++ show duration


benchmarkaddfirst :: Int -> IO ()
benchmarkaddfirst n = do
  let values = makelist n 0  
  start <- getCurrentTime
  let finalArray = runST $ do
        array <- newArray1 n
        forM_ values (addFirst array)  
        showArray array               
  
  finalArray `deepseq` return ()

  end <- getCurrentTime
                 
  let duration = diffUTCTime end start

  putStrLn $ "Time taken to addLast " ++ show n ++ " elements: " ++ show duration

benchmarkinsert :: Int -> IO ()
benchmarkinsert n = do
  let values = makelist n 0              
  start <- getCurrentTime
  let finalArray = runST $ do
        array <- newArray1 n
        forM_ values (insertAt array (div n 2))  
        showArray array               
  finalArray `deepseq` return ()

  end <- getCurrentTime
                  
  let duration = diffUTCTime end start
      

  putStrLn $ "Time taken to addLast " ++ show n ++ " elements: " ++ show duration

main :: IO ()
main = do
     benchmarkaddlast 10000
     benchmarkaddfirst 10000
     benchmarkinsert 10000
