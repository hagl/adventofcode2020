{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as AT
import Data.Functor (($>))
import Data.List
import Data.Text (pack)

main :: IO ()
main = do
  solveFile "src/day24ex.txt"
  solveFile "src/day24.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  putStrLn fileName
  str <- readFile fileName
  let (Right (lines)) = AT.parseOnly programParser (pack str)
  putStrLn "Part 1:"
  let turnedTiles = map head $ filter (\x -> (length x) `mod` 2 == 1) $ groupBy (==) $ sort $ map followLine lines
  print $ length turnedTiles
  solvePart2 turnedTiles

followLine :: [Move] -> (Int, Int)
followLine = foldl' move (0, 0)

move :: (Int, Int) -> Move -> (Int, Int)
move (x, y) m =
  let c = y `mod` 2
      d = 1 - c
   in case m of
        E -> (x + 1, y)
        SE -> (x + c, y + 1)
        SW -> (x - d, y + 1)
        W -> (x - 1, y)
        NE -> (x + c, y - 1)
        NW -> (x - d, y - 1)

solvePart2 :: [(Int, Int)] -> IO ()
solvePart2 start = do
  putStrLn "Part 2:"
  let s = iterate step start !! 100
  print $ length s

step :: [(Int, Int)] -> [(Int, Int)]
step blacks =
  let neighbours = [(move p d) | p <- blacks, d <- [E, SE, SW, W, NE, NW]]
   in map head $ filter isBlack $ groupBy (==) $ sort $ neighbours
  where
    isBlack ns@(h : _) =
      let l = length ns
       in if h `elem` blacks then l == 1 || l == 2 else l == 2

data Move = E | SE | SW | W | NW | NE deriving (Show, Eq)

programParser :: AT.Parser ([[Move]])
programParser = lineParser `AT.sepBy` AT.endOfLine

lineParser :: AT.Parser [Move]
lineParser = many moveParser

moveParser :: AT.Parser Move
moveParser =
  choice
    [ AT.string "e" $> E,
      AT.string "se" $> SE,
      AT.string "sw" $> SW,
      AT.string "w" $> W,
      AT.string "nw" $> NW,
      AT.string "ne" $> NE
    ]