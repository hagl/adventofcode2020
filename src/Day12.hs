module Main where

import Data.List
import Debug.Trace

main :: IO ()
main =
  do
    solveFile "src/day12ex.txt"
    solveFile "src/day12.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let dirs = map parse $ lines str
  let endPos@(x, y, _, _) = foldl' move (0, 0, 1, 0) dirs
  print endPos
  print $ show $ (abs x) + (abs y)
  let endPos2@(x2, y2, _, _) = foldl' move2 (0, 0, 10, -1) dirs
  print endPos2
  print $ show $ (abs x2) + (abs y2)

move2 :: PosDir -> Command -> PosDir
move2 pos@(x, y, dx, dy) c = case c of
  N n -> (x, y, dx, dy - n)
  S n -> (x, y, dx, dy + n)
  E n -> (x, y, dx + n, dy)
  W n -> (x, y, dx - n, dy)
  F n -> (x + (n * dx), y + (n * dy), dx, dy)
  L n -> let (ndx, ndy) = turnR (360 - n) (dx, dy) in (x, y, ndx, ndy)
  R n -> let (ndx, ndy) = turnR n (dx, dy) in (x, y, ndx, ndy)

move :: PosDir -> Command -> PosDir
move pos@(x, y, dx, dy) c = case c of
  N n -> (x, y - n, dx, dy)
  S n -> (x, y + n, dx, dy)
  E n -> (x + n, y, dx, dy)
  W n -> (x - n, y, dx, dy)
  F n -> (x + (n * dx), y + (n * dy), dx, dy)
  L n -> let (ndx, ndy) = turnR (360 - n) (dx, dy) in (x, y, ndx, ndy)
  R n -> let (ndx, ndy) = turnR n (dx, dy) in (x, y, ndx, ndy)

turnR :: Int -> (Int, Int) -> (Int, Int)
turnR 0 (dx, dy) = (dx, dy)
turnR 90 (dx, dy) = (- dy, dx)
turnR 180 (dx, dy) = (- dx, - dy)
turnR 270 (dx, dy) = (dy, - dx)

type PosDir = (Int, Int, Int, Int)

data Command
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int

parse :: String -> Command
parse ('N' : n) = N (read n)
parse ('S' : n) = S (read n)
parse ('E' : n) = E (read n)
parse ('W' : n) = W (read n)
parse ('L' : n) = L (read n)
parse ('R' : n) = R (read n)
parse ('F' : n) = F (read n)
