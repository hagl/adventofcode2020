module Main where

import Data.List

countElem :: (Eq a) => a -> [a] -> Int
countElem x = length . filter ( == x)

valid :: (Int, Int, Char, String) -> Bool
valid (min, max, c, input) = let count = countElem c input in min <= count && count <= max

valid2 :: (Int, Int, Char, String) -> Bool
valid2 (start, end, c, input) = 
  let s = input !! (start - 1)
      e = input !! (end - 1)
  in (s == c) /= (e == c)

countTrees :: [String] -> Int -> Int
countTrees area dx = let
    w = length $ head area
    update (pos, count) line = let
        d = if line !! pos == '#' then 1 else 0 
       in
         (mod (pos + dx) w, count + d)
    (pos, count) = foldl update (0, 0) area
  in
    count

removeLines :: [String] -> [String]
removeLines (a : _ : as) = a : (removeLines as)
removeLines as = as

countAllPaths :: [String] -> Int
countAllPaths area = let
    n1 = countTrees area 1
    n2 = countTrees area 3
    n3 = countTrees area 5
    n4 = countTrees area 7
    n5 = countTrees (removeLines area) 1
  in
    n1 * n2 * n3 * n4 * n5

main :: IO ()
main = do
  print $ countTrees input 3
  print $ countAllPaths input 


input = [
  ".........#..##..#..#........#..",
  "#...#..#..#...##.....##.##.#...",
  "....#..............#....#....#.",
  "#.#..#.....#...#.##..#.#.#.....",
  "........#..#.#..#.......#......",
  ".#........#.#..###.#....#.#.#..",
  "........#........#.......#.....",
  "...##..#.#.#........##.........",
  "#.#.##..###............#...#...",
  "............#....#.......###.##",
  "....##....##..#........#......#",
  "............#.#..........#.....",
  "#.#....#....##...#.....#.....#.",
  "......#.#.#...#.....###....#..#",
  "...........##..#.........#..#.#",
  "..#..#.................#..#..#.",
  ".#....###...#.......#.........#",
  "#.#.#.#...#......#.......#...#.",
  ".......#.#.#...#..............#",
  "...##.......#..##.#.......##...",
  "#.#.##....#..##..##..###...###.",
  ".#......##.##.#....#.##........",
  "..###.............##..##..#....",
  ".....#.#...........#..##..##...",
  ".###.#.#......#.....#........##",
  "...#.......#...##..#..#..#.....",
  "..............#.#..##.##..##..#",
  "#..#.#......#............#.....",
  "........#..#....#..............",
  "...#...#..............#.#####..",
  "...##......#........#.#...#....",
  "..##......#............#..#..#.",
  "....#.........#.#.#.....###.#..",
  "#....#........#........#....#.#",
  ".....#...#..##.....##...#.....#",
  "#...#.#.#...##..##.###.#.#.....",
  "......#.#..........#...#.##....",
  "..............##...#..#.......#",
  "........##.....#.....#.#....#..",
  "..............#..#..#...#.....#",
  "##......##.......##...#.#....#.",
  ".....#.............#.#.........",
  "#.........##..#..#.........##..",
  "..#..#.....#####.........##.#..",
  ".......##.#......#........#....",
  "#.................#.#...#....#.",
  "...#........#.###.##.##.....#..",
  "#.....##..#...##.#.#......#....",
  ".....#..#.#..........##..#.##..",
  "..###.............#..#..#...#..",
  "...###..#...#.....##..........#",
  "#.......#.#...#....#..##..#..#.",
  ".#..#.........#..............#.",
  "..######.....#....##......#....",
  "#..##...#......#..#.#....#.....",
  ".#...................#.#.....#.",
  "..#...#.#..#.#......#..#...#..#",
  "..##..##.#.##.........#.#.#....",
  "...#...#...........#..##.##...#",
  "#...#....#....#....#..#.##..#..",
  "..#.##....#....###..#..........",
  "#.#..##.#.#...##.#..#.##..#.#..",
  "#......##...#.#..........#..#..",
  "#.#...#..#...#.#.#..#........#.",
  "#.#.##.#..#...#..#.#.##........",
  ".....#......#........#..#......",
  "...#....#.#....#...............",
  "....#..###..#....#..#....#....#",
  ".#........###..........##.##.#.",
  "#.#......##....##...##.#......#",
  "#..##.##...#...........##.#.#..",
  ".#.....#.#...#.................",
  "##..........#..#....#.....#...#",
  "....#.#..........##..#.....#.##",
  "#.#..#..#..##..........#.......",
  "..#.#.###......................",
  "......##..##.....#..##.##....#.",
  "...#.......#.##....#......#....",
  "...#...#........#...#.#...#..##",
  "##...#....#.#...#.#.##..##...#.",
  "...#.....#...#...#....###.#..#.",
  "..#.#..#........#......#..##..#",
  "...#......#...#.#.##...##.#.#.#",
  "....#.#....#....#.....#.....##.",
  ".....#.#..##.#....##....##.....",
  ".#...###..#.....#............#.",
  "#..#.#.#..#..#...#....#...#....",
  "#.....#..#...#................#",
  "..........#..#.......#......#.#",
  "...#..#......#...#......#......",
  ".#.#.....#.#.#.#......#..#..#..",
  ".....#.........#.#.#.....##.#..",
  ".....#.#.....#..#..#..#.....###",
  "##....#......##....##.#....#.#.",
  "#####........#..........##.....",
  ".#...##...#...#.......#....#...",
  "#.#.##...##...##..##........#..",
  "#.#..............#.#...#...###.",
  "...#.....##..#.........#....#.#",
  "#.#....#....#..##.#..#...#.....",
  "..#....#.#..#...#...##.....#...",
  "....#...#......................",
  "..#...#.......#..#...##....#...",
  ".#........#...#.....##.##...#..",
  "#......#..............#..#..#..",
  "...........#.#..#.#.#....#....#",
  ".##..##.......#...#..#.....#..#",
  "...#.........#.........###..#..",
  "...#.##....#....#.....#.....#..",
  ".#.#.#.........#.#.#....#....#.",
  "...#..........##..#....#.#.....",
  "...#....##................#....",
  "#....##..#..#........##...#....",
  "#...#...##.#............#....#.",
  "##..#....#...#...............#.",
  "..........#.#...#..##..#.#.....",
  "..##...##..#....#.#......#.....",
  ".......#......#.#.....#.....##.",
  "#...###.....##..##....#.#....#.",
  ".###......#.....#.#............",
  "#.....#.....####.##....#..#....",
  "......###.............#......##",
  ".........##.......##..#..#..#..",
  ".#.......#....#...#...#.#......",
  "#...#..#...#........#...##..#..",
  ".#....#........#.........##..#.",
  "..............##.#...##..#.##.#",
  ".#....#...#....#......#..#.....",
  "#....##.#...#.#.....###..#....#",
  "#.......##.#..###..............",
  "#..#..#..#......#.#..#...#..#.#",
  ".......#.#.#..#..#...#..#......",
  ".#..#......#.....#......##..##.",
  "....#....#.......#.......#.#.##",
  ".......#.#................#...#",
  "#.#.....#.......#.#........#...",
  ".....#....##...#......#.....##.",
  ".#......#.#...#..#....#....#.##",
  "##...#.###.#....#..#....#.#...#",
  "....#.##..##.#.............#...",
  "#..#.............##.......#.#..",
  "##.#..#..#.#...........###...##",
  ".#.#.....#......###........#...",
  "#.#...#.#....##......#.#....#..",
  "#.........#..........#.........",
  ".......#....#...#..#.....#...##",
  ".......................#...#..#",
  ".###...........##...#........##",
  "#.#....######.#........#..##.#.",
  "..#.##.#...#.#.......#.##.##..#",
  "#.............###..#.##.#......",
  "...#..##......#...#..###.....#.",
  "..........#.....#..#...##..#...",
  "..##..........#.#..#.....#...#.",
  "...#.......#.....##.........#..",
  "#..#.#...#..#...###...#...#.#..",
  "#.##....#..#.#.......#..#..#...",
  "..#.##.#......#.#......#....#..",
  "..........#...##.....###.......",
  "...#...##..#......#...##.......",
  "....#........#.#.......#..###..",
  ".....#.#..........##.#..#..#.#.",
  ".............##.....#.#..##....",
  "...#...............##...#......",
  "....#......#..#....#...##..#...",
  ".##.#....#.#.....#.#.........#.",
  ".....#.###....#..###..#.#.....#",
  ".#.........##.........##...#...",
  "..#.....###....##..........#..#",
  "........#..#.#.#..#.......#..##",
  "..#.#..#.#............#.##.#..#",
  ".#....#.....#..#...#.......##..",
  ".#...........#.#..#..###.###...",
  "..#.....#..#........#.#........",
  ".#........##........#..#.##....",
  "......#.....##........##..#....",
  ".#..................##....#.#..",
  ".#..#.#..#.#...#........#......",
  "...#..##.#......#..#..........#",
  "....#.##...#....##.............",
  "#....#.##....##.###..#..#..#...",
  "..........#..#...##.##....#..#.",
  ".###.#.....#...#...#...#.......",
  "............#...............#.#",
  "#....#...#......#....#.#.#.#.##",
  "...#..........#.#.#.....###....",
  "#.#...##...#..#.....###...#....",
  "......#...#..#..#..#.##...##...",
  "...#..#.#....#...#.#.........##",
  "##....#..###.#.##.....##.......",
  "..#.#...#..##.......#.#.......#",
  "##......#...........#......#...",
  ".......#..###....###..##.#...##",
  ".........#.....#..#.......##..#",
  ".......#.##..#....#...#.#...#..",
  "#..#.#..................##.#..#",
  "...#..#..#.....#..#........#...",
  "...#.#..###..#.....##...#....#.",
  "..#..#......#...........#...#..",
  "#...##.##..###.......##........",
  ".#.....#..#....#.....#.##....#.",
  "#..#........#.#....#..#...#.###",
  "..#...#.#.#.....#.....#..#.....",
  ".##.............#.#......##...#",
  ".#....#####............#.....##",
  "#.###.......#.#...##.....#.....",
  "......#.##..#...#..#..##.#..##.",
  "......#.#...##.....#...#....##.",
  "....#............#...#...#....#",
  ".........##.#.#....#....#....##",
  ".#...##.#...#.......#.##....#.#",
  "#....#.#...#.#...#.#.#...#.....",
  ".#.#.........##..#..#..........",
  ".#.........#.#.....#..#.#..###.",
  "....##.#.#..........#..####....",
  "....#..#.#.#...#...#..#....#...",
  "..#.#...#...##.......#.#.#..#..",
  "...##...#......#.....#.#...#..#",
  "......#.###.#.......##...#...#.",
  ".....#.#.#......##..........###",
  "##.#.#.#..#....#...............",
  ".#.#.##.......#....#.#.....#..#",
  ".........#...#.#..#.......#....",
  "....#.####.#......#...#...##...",
  "#..#..#..#..#....#...##.....##.",
  "......####.#..##..#.....##.....",
  "##.#.........#........#..#.#...",
  ".#.#....#....#.......#.#....##.",
  "....#....#.......##..#.....#...",
  ".#......#..#....#.#............",
  "#..#.#.##.....#..#.#.#.#.#.##..",
  ".#.....#.....#...#..#.#...#.#..",
  ".#.#.##............#.#.#.#.#.#.",
  ".##..........#.....#...###.....",
  "#.#...#...#................#.#.",
  "##...#.##.....#.....#.#.##.....",
  "####.....##..........#......#..",
  "#.............#..............#.",
  ".###....#.#...#..#..#..#.......",
  "..#.#.....#...#..#..####.......",
  "...#.#..#........#..##..#..#.##",
  ".#........#..........#.#...##..",
  ".#.......#.#.#..#...#..#.#...##",
  ".#.....##......##..............",
  "......#..#.#.##...##.#.....#...",
  ".........#.#...##.....##....#.#",
  ".....##...#........#..#.#..#.#.",
  ".#.##..#.....##...#...###.#.#..",
  "...##...#...#..#.#..#..........",
  "##..............#...#...#.#..#.",
  "......#..#......#..#.....#...#.",
  ".......#...#..#....#.....#.....",
  "..##.....##..#.#........#......",
  ".###.#...#.....................",
  "..#...#.................#...#..",
  "#..#.##...####.............#...",
  ".##....#..####.......#.........",
  "#..#...###...#...#..#..##......",
  "....#.##.#.#.........#.....#..#",
  ".....#...#.....#.#.#.##.#...##.",
  ".............#........#.....#..",
  "...##.###.#....##.......#..#...",
  "#..#....#....#.#............#..",
  ".........#.##........##.....#..",
  ".........#.#.#..#..#.......#...",
  ".......#.#..#.......#.....#.#..",
  "##.#.....##...##.....#.#.......",
  ".#.#.#......##.##.#.........#..",
  "..#.##..###.....###.........##.",
  ".#......#..#..##...#.#...##.#.#",
  "......#.#............#.....#...",
  "###.#..#..#..#..#.##...#.......",
  ".#.#.##..###....#......##..###.",
  "#...#.#.#..#..#..##.#.##....#..",
  "..#...#...####...#......####.##",
  "..##.#.####........#..#......#.",
  ".#..#.......#...#.#.........#..",
  "........#.#....#..#####..#.....",
  ".#...........#..#..#..#...#....",
  "....#....#...#.................",
  "....##..#....##....#..#....#.##",
  "....#.##.....###...#...##.##...",
  "......##.#..##.#.#.#....#.#.#..",
  "##.#...###....#.#..#.#.###....#",
  "......###..#..#..........##...#",
  "..........#.##...##..#....##.#.",
  ".#...#.#..#.#.#..#.....#.......",
  ".#....#..#.#..#.#...##.#.#.....",
  ".##.....#...#..##.#........#...",
  "....#......#.........#....#..##",
  ".#..#.#.#.#..#..#.#.........#..",
  ".........#.....#...#....#......",
  "#..#..#........#...#.#.........",
  "...#.#.#...##.#.#...#..#......#",
  "#.#.#.#........#...#..#.....#..",
  ".###..#..#..###..#..#..........",
  ".....#......#.#..#...#.......#.",
  "##.##.........#.......##.......",
  "#...##.......#..#.#.......#....",
  "#..#..#.....#...#......#.......",
  ".#..#..#.##....#.#..#...#...#..",
  ".#...#.....#..#.........#..#...",
  "...#.#.#.......#....#..##.....#",
  ".........#..##.#..#..#.#.......",
  "#.##.....##..###..#..#..#.##...",
  "........#......#...##...###..##",
  ".##....##..#..#..###......#....",
  "............##......#...#..##..",
  "...##.......#......#...##.##..#",
  "...#..#..#.#...####.#.......#..",
  "..#.##..#....#......#.#.....#..",
  "..#.##..............#..##.....#",
  ".....##....#......#....#......#",
  "......#..#......#.........#..#.",
  "...#.##.###...###..#.##........",
  "..........####.#.##.....#..#.##",
  "#...##...........#...........##",
  "#.#..#.#....#.#..#....##......#",
  ".......#...#.....#......#.#.##.",
  "....#.##..##..........#..#.....",
  "#.#.#...#......#...#.....#.##.#",
  "........#.......#..##.....##...",
  ".....####.#....#.#............."
  ]