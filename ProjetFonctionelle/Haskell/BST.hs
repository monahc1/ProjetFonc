import System.Random
import Control.Monad
import Data.Time.Clock
import qualified Data.List as List

data BST a = Empty | Node a (BST a) (BST a) deriving (Show)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node a left right)
    | x < a     = Node a (insert x left) right
    | x > a     = Node a left (insert x right)
    | otherwise = Node a left right

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

randomList :: Int -> IO [Int]
randomList n = replicateM n $ randomRIO (1, 1000000)

benchmarkInsertion :: Int -> IO ()
benchmarkInsertion n = do
    values <- randomList n
    start <- getCurrentTime
    let bst = foldr insert Empty values
    end <- bst `seq` getCurrentTime
    let duration = diffUTCTime end start
    putStrLn $ "Time taken to insert " ++ show n ++ " elements: " ++ show duration ++ " seconds"

main :: IO ()
main = do
    benchmarkInsertion 100000
    benchmarkInsertion 200000
    benchmarkInsertion 500000