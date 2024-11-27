import System.Random (newStdGen, randomRs)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.DeepSeq (deepseq)
import qualified Data.IntMap.Strict as IntMap
import Control.Exception (evaluate)
import Data.List (foldl')


type Graph = IntMap.IntMap [Int]


createGraph :: Int -> Graph
createGraph vertices = IntMap.fromList $ zip [0..vertices-1] (repeat [])


addEdge :: Int -> Int -> Graph -> Graph
addEdge u v graph = IntMap.adjust (v:) u graph


buildGraph :: Int -> Int -> IO Graph
buildGraph vertices edges = do
  gen <- newStdGen
  let pairs = take edges $ zip (randomRs (0, vertices-1) gen) (randomRs (0, vertices-1) gen)
  return $ foldr (uncurry addEdge) (createGraph vertices) pairs


estimateMemory :: Graph -> Int
estimateMemory graph = 
  let vertexBytes = IntMap.size graph * 20 
      edgeBytes = foldl' (\acc lst -> acc + 24 * length lst) 0 graph 
  in vertexBytes + edgeBytes


benchmarkGraph :: Int -> Int -> IO ()
benchmarkGraph vertices edges = do
  start <- getCurrentTime
  graph <- buildGraph vertices edges
  graph `deepseq` return ()
  end <- getCurrentTime
  let duration = diffUTCTime end start
  let memory = estimateMemory graph
  putStrLn $ "Benchmark Results for " ++ show vertices ++ " vertices and " ++ show edges ++ " edges:"
  putStrLn $ "Time taken: " ++ show duration ++ " seconds"
  putStrLn $ "Estimated memory usage: " ++ show memory ++ " bytes"
  putStrLn $ "--------------------------------------------------"

main :: IO ()
main = do
  benchmarkGraph 1000 5000
  benchmarkGraph 2000 10000
  benchmarkGraph 5000 25000
