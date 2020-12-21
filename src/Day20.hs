{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import qualified Data.HashMap.Strict as M
import Data.List (find, findIndex, foldl', groupBy, init, intercalate, isPrefixOf, nub, sort, sortOn, transpose)
import Data.List.Split
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)
import Debug.Trace
import GHC.Base (assert)
import Text.RE.TDFA.String

main :: IO ()
main = do
  solveFile "src/day20ex.txt"
  solveFile "src/day20.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let tiles = parseTiles $ lines str
  print $ length tiles
  -- print $ head tiles
  let s = tiles >>= (nub . sort . (map head) . sides)
  let uniques = map head $ filter (\l -> length l == 1) $ groupBy (==) $ sort s
  print uniques
  print $ length uniques
  let corners = filter (isCorner uniques) tiles
  -- print corners
  print $ product $ map num corners
  solve2 tiles uniques corners

solve2 :: [Tile] -> [Int] -> [Tile] -> IO ()
solve2 tiles uniques corners = do
  putStrLn $ unlines $ map show $ (map sides tiles)
  let start = (head corners, findStart (head corners) uniques)
  let (top, rest) = calculateLine tiles start
  print (length top, length rest)
  let puzzle = calculatePuzzle tiles start
  let assembled = assemblePuzzle puzzle
  putStrLn $ unlines $ assembled
  print $ map (countRocks . removeMonster) $ map (\i -> posToTurn i assembled) [0 .. 7]
  -- putStrLn $ unlines $ posToTurn 3 assembled
  putStrLn $ unlines $ removeMonster $ posToTurn 5 assembled
  print $ countRocks $ removeMonster $ posToTurn 5 assembled

-- putStrLn $ unlines $ solution

assemblePuzzle :: [[(Tile, Int)]] -> [String]
assemblePuzzle puzzle = puzzle >>= assembleLines

assembleLines :: [(Tile, Int)] -> [String]
assembleLines = (map concat) . transpose . (map assembleTile)

assembleTile :: (Tile, Int) -> [String]
assembleTile (tile, ix) = map init $ map tail $ init $ tail $ posToTurn ix (matrix tile)

calculatePuzzle :: [Tile] -> (Tile, Int) -> [[(Tile, Int)]]
calculatePuzzle tiles start =
  let (line, remaining) = calculateLine tiles start
   in --trace ("*** " ++ (show $ length remaining) ++ "* " ++ (show $ map (show . num . fst) line)) $
      case remaining of
        [] -> [line]
        _ ->
          let nextStart = findBelow (last line) remaining
           in line : (calculatePuzzle remaining (fromJust nextStart))

calculateLine :: [Tile] -> (Tile, Int) -> ([(Tile, Int)], [Tile])
calculateLine tiles start@(t0, _) = go (filter (/= t0) tiles) start [start]
  where
    go remaining right acc = case findLeft right remaining of
      Nothing -> (acc, remaining)
      Just f@(t, i) -> go (filter (/= t) remaining) f (f : acc)

findLeft :: (Tile, Int) -> [Tile] -> Maybe (Tile, Int)
findLeft (tile, ix) tiles =
  let leftSide = (sides tile) !! ix !! 3
   in find (\(t, i) -> (sides t) !! i !! 1 == leftSide) [(t, i) | t <- tiles, i <- [0 .. 7]]

findBelow :: (Tile, Int) -> [Tile] -> Maybe (Tile, Int)
findBelow (tile, ix) tiles =
  let bottomSide = (sides tile) !! ix !! 2
   in find (\(t, i) -> (sides t) !! i !! 0 == bottomSide) [(t, i) | t <- tiles, i <- [0 .. 7]]

findStart :: Tile -> [Int] -> Int
findStart t uniques = fromJust $ findIndex topLeftUniq $ sides t
  where
    topLeftUniq [a, b, _, _] = a `elem` uniques && b `elem` uniques

sidesOfTile :: Tile -> [Int]
sidesOfTile = (nub . sort . (map head) . sides)

isCorner :: [Int] -> Tile -> Bool
isCorner uniques tile =
  let s = sidesOfTile tile
      us = filter (\x -> x `elem` uniques) s
   in length us == 4

data Tile = Tile
  { num :: Int,
    matrix :: [String],
    -- 8 variatons
    -- [orig, turned 90, turned 180, turned 270,
    -- flipped horizontal, flipped horizontal turned 90, flipped horizontal turned 180, flipped horizontal turned 270]
    sides :: [[Int]]
  }
  deriving (Eq, Show)

parseTiles :: [String] -> [Tile]
parseTiles = (map parseTile) . (chunksOf 12)

parseTile :: [String] -> Tile
parseTile (h : t) =
  let num = read $ drop 5 $ init h
      matrix = take 10 t
   in Tile num matrix (calculateSidesVariations matrix)

calculateSidesVariations :: [String] -> [[Int]]
calculateSidesVariations orig = map (\f -> calculateSides $ f orig) $ map posToTurn [0 .. 7]

turn = (map reverse) . transpose

posToTurn :: Int -> [String] -> [String]
posToTurn 0 = id
posToTurn 1 = turn
posToTurn 2 = turn . turn
posToTurn 3 = turn . turn . turn
posToTurn 4 = reverse
posToTurn 5 = turn . reverse
posToTurn 6 = turn . turn . reverse
posToTurn 7 = turn . turn . turn . reverse

calculateSides :: [String] -> [Int]
calculateSides ss =
  [ calculateSide $ head ss,
    calculateSide $ map last ss,
    calculateSide $ last ss,
    calculateSide $ map head ss
  ]

calculateSide :: String -> Int
calculateSide = foldl' (\acc c -> 2 * acc + if c == '#' then 1 else 0) 0

monster :: [String]
monster = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]

mIx0 = map snd $ filter (\(c, _) -> c == '#') $ zip (monster !! 0) [0 ..]

mIx1 = map snd $ filter (\(c, _) -> c == '#') $ zip (monster !! 1) [0 ..]

mIx2 = map snd $ filter (\(c, _) -> c == '#') $ zip (monster !! 2) [0 ..]

countRocks :: [String] -> Int
countRocks s = length $ filter (== '#') $ concat s

removeMonster :: [String] -> [String]
removeMonster (a : b : c : rest) = let (a', b', c') = removeMonsterLine (a, b, c) in a' : removeMonster (b' : c' : rest)
removeMonster rest = rest

removeMonsterLine :: (String, String, String) -> (String, String, String)
removeMonsterLine (a, b, c) = foldl' removeMonsterLine' (a, b, c) [0 .. ((length a) - 20)]

removeMonsterLine' :: (String, String, String) -> Int -> (String, String, String)
removeMonsterLine' (a, b, c) z =
  if (mm z a mIx0) && (mm z b mIx1) && (mm z c mIx2)
    then (rm z a mIx0, rm z b mIx1, rm z c mIx2)
    else (a, b, c)

mm :: Int -> String -> [Int] -> Bool
mm z s ix = all (\i -> (s !! (z + i)) == '#') ix

rm :: Int -> String -> [Int] -> String
rm z s ixs = reverse $ go s 0 ""
  where
    go :: String -> Int -> String -> String
    go [] _ acc = acc
    go (c : cs) i acc = go cs (i + 1) ((if ((i - z) `elem` ixs) then 'O' else c) : acc)

solution =
  [ ".####...#####..#...###..",
    "#####..#..#.#.####..#.#.",
    ".#.#...#.###...#.##.O#..",
    "#.O.##.OO#.#.OO.##.OOO##",
    "..#O.#O#.O##O..O.#O##.##",
    "...#.#..##.##...#..#..##",
    "#.##.#..#.#..#..##.#.#..",
    ".###.##.....#...###.#...",
    "#.####.#.#....##.#..#.#.",
    "##...#..#....#..#...####",
    "..#.##...###..#.#####..#",
    "....#.##.#.#####....#...",
    "..##.##.###.....#.##..#.",
    "#...#...###..####....##.",
    ".#.##...#.##.#.#.###...#",
    "#.###.#..####...##..#...",
    "#.###...#.##...#.##O###.",
    ".O##.#OO.###OO##..OOO##.",
    "..O#.O..O..O.#O##O##.###",
    "#.#..##.########..#..##.",
    "#.#####..#.#...##..#....",
    "#....##..#.#########..##",
    "#...#.....#..##...###.##",
    "#..###....##.#...##.##.#"
  ]