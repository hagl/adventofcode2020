module Main where

import Data.List

main :: IO ()
main =
  do
    solveFile "src/day10ex.txt"
    solveFile "src/day10ex2.txt"
    solveFile "src/day10.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let inputs = (map read $ lines str) :: [Int]
  print $ length inputs
  print $ solve inputs
  print $ solve2 inputs
  print ""

solve :: [Int] -> Int
solve list =
  let tl = 0 : (3 + (maximum list)) : list
      sorted = sort tl
      zipped = zip sorted (tail sorted)
      diffs = map (\(x, y) -> y - x) zipped
      d1 = length $ filter (== 1) diffs
      d3 = length $ filter (== 3) diffs
   in d1 * d3

solve2 :: [Int] -> Int
solve2 list =
  let m = 3 + (maximum list)
      tl = 0 : m : list
      go2 [a, b, c] n = [b, c, (if n `elem` tl then a + b + c else 0)]
      [_, _, res] = foldl' go2 [0, 0, 1] [1 .. m]
   in res
