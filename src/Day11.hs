module Main where

import Data.List
import Debug.Trace

main :: IO ()
main =
  do
    solveFile "src/day11ex.txt"
    solveFile "src/day11.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let ls = lines str
  let initial = Grid ((length (head ls)) - 1, (length ls) - 1) ls
  print $ length $ filter (== '#') $ concat $ g $ run initial 0
  print $ length $ filter (== '#') $ concat $ g $ run2 initial 0

data Grid = Grid {bounds :: (Int, Int), g :: [String]} deriving (Show, Eq)

neighbours :: Grid -> (Int, Int) -> [Char]
neighbours a (cx, cy) =
  let (ux, uy) = bounds a
   in [(g a) !! y !! x | x <- [(max 0 (cx - 1)) .. (min ux (cx + 1))], y <- [(max 0 (cy - 1)) .. (min uy (cy + 1))], (x, y) /= (cx, cy)]

step :: Grid -> Grid
step a =
  let b@(ux, uy) = bounds a
      update p@(x, y) = case (g a) !! y !! x of
        '.' -> '.'
        'L' -> if (find (== '#') (neighbours a p)) == Nothing then '#' else 'L'
        '#' -> if (length (filter (== '#') (neighbours a p)) >= 4) then 'L' else '#'
   in Grid b $! [[update (x, y) | x <- [0 .. ux]] | y <- [0 .. uy]]

run :: Grid -> Int -> Grid
run a c =
  let b = step $! a
   in if (a == b) then a else run b (c + 1)

occupied :: Grid -> (Int, Int) -> Int
occupied a (cx, cy) =
  let dirs = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]
   in sum $ map (occupiedDir a (cx, cy)) dirs

occupiedDir :: Grid -> (Int, Int) -> (Int, Int) -> Int
occupiedDir a (x, y) d@(dx, dy) =
  let (bx, by) = bounds a
      n@(nx, ny) = (x + dx, y + dy)
   in if (nx < 0 || ny < 0 || nx > bx || ny > by)
        then 0
        else case (g a) !! ny !! nx of
          'L' -> 0
          '#' -> 1
          '.' -> occupiedDir a n d

step2 :: Grid -> Grid
step2 a =
  let b@(ux, uy) = bounds a
      update p@(x, y) = case (g a) !! y !! x of
        '.' -> '.'
        'L' -> if (occupied a p) == 0 then '#' else 'L'
        '#' -> if (occupied a p) >= 5 then 'L' else '#'
   in Grid b $! [[update (x, y) | x <- [0 .. ux]] | y <- [0 .. uy]]

run2 :: Grid -> Int -> Grid
run2 a c =
  let b = step2 $! a
   in if (a == b) then a else run2 b (c + 1)
