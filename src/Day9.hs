{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)
import Debug.Trace

main :: IO ()
main =
  do
    strEx <- readFile "src/day9ex.txt"
    let inputsEx = (map read $ lines strEx) :: [Int]
    print $ length inputsEx
    print $ findMismatch 5 (take 5 inputsEx) (drop 5 inputsEx)

    str <- readFile "src/day9.txt"
    let inputs = (map read $ lines str) :: [Int]
    print $ length inputs
    print $ findMismatch 25 (take 25 inputs) (drop 25 inputs)
    let Just cl = findRange 31161678 inputs
    print $ (maximum cl) + (minimum cl)

findRange :: Int -> [Int] -> Maybe [Int]
findRange s [] = Nothing
findRange s l = case (sumIntro s l) of
  Nothing -> findRange s (tail l)
  Just x -> Just x

sumIntro :: Int -> [Int] -> Maybe [Int]
sumIntro s l = go s l []
  where
    go 0 _ acc = Just acc
    go s [] acc = Nothing
    go n (y : ys) acc = if (n - y) < 0 then Nothing else go (n - y) ys (y : acc)

sumOfTwo :: Int -> [Int] -> Bool
sumOfTwo x [] = False
-- sumOfTwo x l@(y : ys) = trace ((show x) ++ (show l)) $ (x - y) /= y && (x - y) `elem` ys || sumOfTwo x ys
sumOfTwo x l@(y : ys) = (x - y) /= y && (x - y) `elem` ys || sumOfTwo x ys

findMismatch :: Int -> [Int] -> [Int] -> Maybe Int
findMismatch s acc [] = Nothing
findMismatch s acc (x : xs) = if (sumOfTwo x acc) then findMismatch s (x : (take s acc)) xs else Just x

-- str <- readFile "src/day9.txt"
-- let (Right instructions) = parseOnly programParser (pack str)
-- print $ length instructions
-- print $ show (run instructions)
-- print $ show (modify instructions)