module Main where

import Data.List

main :: IO ()
main = 
  do 
    str <- readFile "src/day5.txt"
    let seats = map (\(a,b) -> a * 8 + b) $ map toCoor $ lines str
    let sorted = sort seats
    let pairs = zip sorted (drop 1 sorted)
    let Just (left, right) = find (\(a,b) -> b-a > 1) pairs
    print $ maximum seats
    print $ left + 1

toCoor :: String -> (Int, Int) 
toCoor s = let
    row = toDec $ take 7 s
    col = toDec $ drop 7 s
  in 
    (row, col)
  where 
    toBin 'B' = 1
    toBin 'F' = 0
    toBin 'R' = 1
    toBin 'L' = 0
    toDec = foldl' (\acc x -> acc * 2 + toBin x) 0
