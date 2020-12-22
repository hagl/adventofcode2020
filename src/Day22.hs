{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as AT
import qualified Data.HashMap.Strict as M
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)
import Debug.Trace

main :: IO ()
main = do
  solveFile "src/day22ex.txt"
  solveFile "src/day22.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  putStrLn fileName
  str <- readFile fileName
  let (Right (p1, p2)) = AT.parseOnly programParser (pack str)
  putStrLn "Part 1:"
  let (r1, r2) = play p1 p2
  print $ score $ r1 ++ r2
  putStrLn "Part 1:"
  let (s1, s2) = play2 p1 p2
  print $ score $ s1 ++ s2

score :: [Int] -> Int
score = sum . map (\(a, b) -> a * b) . zip [1 ..] . reverse

play :: [Int] -> [Int] -> ([Int], [Int])
play p1 p2 = go p1 [] p2 []
  where
    go [] [] p2 r2 = ([], p2 ++ (reverse r2))
    go p1 r1 [] [] = (p1 ++ (reverse r1), [])
    go [] r1 p2 r2 = go (reverse r1) [] p2 r2
    go p1 r1 [] r2 = go p1 r1 (reverse r2) []
    go (c1 : cs1) r1 (c2 : cs2) r2 = if (c1 > c2) then go cs1 (c2 : c1 : r1) cs2 r2 else go cs1 r1 cs2 (c1 : c2 : r2)

play2 :: [Int] -> [Int] -> ([Int], [Int])
play2 p1 p2 = go p1 p2 []
  where
    go [] p2 acc = ([], p2)
    go p1 [] acc = (p1, [])
    go p1@(c1 : cs1) p2@(c2 : cs2) acc =
      let pair = (p1, p2)
          newAcc = (pair : acc)
       in if (pair `elem` acc)
            then (p1, [])
            else
              if (c1 > length cs1 || c2 > length cs2)
                then if (c1 > c2) then go (cs1 ++ [c1, c2]) cs2 newAcc else go cs1 (cs2 ++ [c2, c1]) newAcc
                else case play2 (take c1 cs1) (take c2 cs2) of
                  ([], _) -> go cs1 (cs2 ++ [c2, c1]) newAcc
                  (_, []) -> go (cs1 ++ [c1, c2]) cs2 newAcc

programParser :: AT.Parser ([Int], [Int])
programParser = do
  AT.string "Player 1:"
  AT.endOfLine
  p1 <- AT.decimal `AT.sepBy` AT.endOfLine
  AT.endOfLine
  AT.endOfLine
  AT.string "Player 2:"
  AT.endOfLine
  p2 <- AT.decimal `AT.sepBy` AT.endOfLine
  return (p1, p2)
