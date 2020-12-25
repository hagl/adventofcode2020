{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.List
import Data.Text (pack)

main :: IO ()
main = do
  solveFile "src/day25ex.txt"
  solveFile "src/day25.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  putStrLn fileName
  str <- readFile fileName
  let [card, door] = map read $ lines str :: [Int]
  putStrLn "Part 1:"
  print card
  print door
  let cardLS = findLoopSize 7 card
  print $ encrypt cardLS door

m = 20201227

findLoopSize :: Int -> Int -> Int
findLoopSize sn result = go 1 0
  where
    go acc count = if acc == result then count else go ((acc * sn) `mod` m) (count + 1)

encrypt :: Int -> Int -> Int
encrypt ls val = go val ls
  where
    go acc 1 = acc
    go acc c = go ((acc * val) `mod` m) (c - 1)