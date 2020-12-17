module Main where

import Control.Monad
import qualified Data.HashMap.Strict as M
import Data.Ix
import Data.List (find, foldl', isPrefixOf, sortOn, transpose)

main :: IO ()
main = do
  solveFile "src/day17ex.txt"
  solveFile "src/day17.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let matrix = readInput $ lines str
  print matrix
  print $ dimensions matrix

  -- putStrLn $ printMatrix matrix
  -- putStrLn $ printMatrix $ step $ step matrix
  let sixTimes = foldl' (\s _ -> step s) matrix [1 .. 6]
  putStrLn $ printMatrix sixTimes
  print $ length $ filter (== True) $ M.elems sixTimes

type Space = M.HashMap (Int, Int, Int) Bool

readInput :: [String] -> Space
readInput list = go list 0 M.empty
  where
    go [] _ acc = acc
    go (l : ls) y acc = go ls (y + 1) (go2 l 0 y acc)
    go2 [] _ _ acc = acc
    go2 (c : cs) x y acc = go2 cs (x + 1) y (if c == '#' then M.insert (x, y, 0) True acc else acc)

dimensions :: Space -> ((Int, Int), (Int, Int), (Int, Int))
dimensions s =
  let ks = M.keys s
      xs = map (\(a, _, _) -> a) ks
      ys = map (\(_, b, _) -> b) ks
      zs = map (\(_, _, c) -> c) ks
   in ((minimum xs, maximum xs), (minimum ys, maximum ys), (minimum zs, maximum zs))

printMatrix :: Space -> String
printMatrix s =
  let ((x1, x2), (y1, y2), (z1, z2)) = dimensions s
      printLetter z y x = if M.lookup (x, y, z) s == Just True then '#' else '.'
      printLine z y = map (printLetter z y) [x1 .. x2]
      printSlice z = ("z=" ++ show z) : (map (printLine z) [y1 .. y2])
   in unlines $ [z1 .. z2] >>= printSlice

step :: Space -> Space
step s =
  let ((x1, x2), (y1, y2), (z1, z2)) = dimensions s
      ixs = [(x, y, z) | x <- [(x1 - 1) .. (x2 + 1)], y <- [(y1 - 1) .. (y2 + 1)], z <- [(z1 - 1) .. (z2 + 1)]]
   in foldl' (checkNeighbours s) M.empty ixs

neighbours = [(x, y, z) | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], (x, y, z) /= (0, 0, 0)]

checkNeighbours :: Space -> Space -> (Int, Int, Int) -> Space
checkNeighbours oldSpace acc p@(x, y, z) =
  let values = map (\(a, b, c) -> (M.lookup (a + x, b + y, c + z) oldSpace)) neighbours
      count = length $ filter (== Just True) values
      origValue = M.lookup p oldSpace
      newValue = rule (origValue == Just True) count
   in M.insert p newValue acc

rule :: Bool -> Int -> Bool
rule True 2 = True
rule _ 3 = True
rule _ _ = False