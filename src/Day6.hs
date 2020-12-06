module Main where

import qualified Data.Set as S

main :: IO ()
main = 
  do 
    str <- readFile "src/day6.txt"
    let ls = lines str
    print $ solve1 ls
    print $ solve2 ls

solve1 :: [String] -> Int
solve1 ls = let
    groups = combineLines ls
    uniqs = map (length . S.toList . S.fromList) groups
  in 
    sum uniqs
  where
    combineLines = foldl addLine [""]
    addLine ls "" = "" : ls
    addLine (l:ls) c = (l ++ c ):ls

solve2 :: [String] -> Int
solve2 ls = let
    groups = combineLines ls
    uniqs = map (length . S.toList) groups
  in 
    sum uniqs
  where
    combineLines = foldl addLine [S.fromList "abcdefghijklmnopqrstuvwxyz"]
    addLine ls "" = (S.fromList "abcdefghijklmnopqrstuvwxyz") : ls
    addLine (l:ls) c = (S.intersection l (S.fromList c)):ls    