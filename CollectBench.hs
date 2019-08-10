{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified XSaiga.Getts as Getts
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified System.Random as Rand
import System.CPUTime
import Text.Printf
import Control.Exception
import System.Environment
import Data.Foldable
import qualified Data.List as List

wale_collect = Map.toList . Map.fromListWith (++) . map (\ (x,y) -> (x,[y]))

solarman_collect = Getts.collect

unstableSortFirst = Seq.unstableSortBy (\x y -> compare (fst x) (fst y))

solarman_collect' = Getts.condense . toList . unstableSortFirst . Seq.fromList

solarman_collect'' = Getts.condense . List.sortOn fst

gangster_collect [] = []
gangster_collect ((x,y):t) =
        (x, y:[e2 | (e1, e2) <- t, e1 == x]) :
        gangster_collect [(e1, e2) | (e1, e2) <- t, e1 /= x]


nEvents = 10000
nEntities = 1000
--nTuples = 1000000

--Use 1024 as seed -- want deterministic results
gen = Rand.split $ Rand.mkStdGen 1024

gen_events = fst gen
gen_entities = snd gen

events :: Int -> [Int]
events nTuples  = take nTuples $ Rand.randomRs (1, nEvents) gen_events

entities :: Int -> [Int]
entities nTuples = take nTuples $ Rand.randomRs (1, nEntities) gen_entities

tuples nTuples = zip (map show $ entities nTuples) (map (\x -> "event" ++ show x) $ events nTuples)

repeatedEntities :: Int -> [Int]
--generate a list [1..nEntities] such that each element is repeated, in order
--enough times so that length repeatedEntities == nTuples
--assumes that nEntities divides nTuples
repeatedEntities nTuples =
  concatMap (replicate n) [1..nEntities]
  where
    n = nTuples `div` nEntities

sortedTuples nTuples = zip (map show $ repeatedEntities nTuples) (map (\x -> "event" ++ show x) $ events nTuples)

time impl t = do
  start <- getCPUTime
  mapM_ (\(x, y) -> evaluate (evaluate x, mapM_ evaluate y)) $ impl $ tuples t
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.9f sec\n" (diff :: Double)

timeSorted impl t = do
  start <- getCPUTime
  --mapM_ (\(x, y) -> evaluate (evaluate x, mapM_ evaluate y)) $ impl $ sortedTuples t
  evaluate $ last $ impl $ sortedTuples t
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.9f sec\n" (diff :: Double)


selectF "wale_collect" = wale_collect
selectF "solarman_collect" = solarman_collect
selectF "solarman_collect''" = solarman_collect''
selectF "solarman_collect'" = solarman_collect'
selectF "gangster_collect" = gangster_collect
selectF "Getts.collect" = Getts.collect
selectF "Getts.condense" = Getts.condense

main = do
  [n, t, s] <- getArgs
  let collect = selectF n
  let tFunc = selT s
  tFunc collect $ read t
  where
    selT "--sorted" = timeSorted
    selT "--unsorted" = time

--Generate 100000 pairs of strings

--main = do
--  let seed = 1024
--  let gen = Rand.mkStdGen seed
--  let rs = Rand.randoms gen
--  print (take 100 rs)
