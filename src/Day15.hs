{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Bits
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as M
import Data.List
import Data.List.Split
import Debug.Trace
import Text.RE.TDFA.String

main :: IO ()
main =
  do
    solveFile "src/day15ex.txt"
    solveFile "src/day15.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let list = map read $ splitOn "," str :: [Int]
  let s@(cache, count, last) = initCache M.empty 1 list
  print s
  let (_, result) = run (cache, count, last)
  print $ result

type Cache = M.HashMap Int Int

initCache :: Cache -> Int -> [Int] -> (Cache, Int, Int)
initCache cache count [last] = (cache, count, last)
initCache cache count (a : as) = let newCache = M.insert a count cache in initCache newCache (count + 1) as

run :: (Cache, Int, Int) -> (Cache, Int)
run (cache, 2020, last) = (cache, last)
run (cache, count, last) =
  let n = case M.lookup last cache of
        Nothing -> 0
        Just c -> count - c
   in run (M.insert last count cache, (count + 1), n)
