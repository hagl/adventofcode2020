{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Bits
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Text.RE.TDFA.String

main :: IO ()
main =
  do
    -- solveFile "src/day14ex.txt"
    solveFile "src/day14ex2.txt"
    solveFile "src/day14.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let result = runProg $ lines str
  print $ result
  let result2 = runProg2 $ lines str
  print $ result2

type Mem = M.Map Int Int

type Mask = (Int, Int)

runProg :: [String] -> Integer
runProg prog =
  let mem = execute prog (0, 0) M.empty
   in sum $ map (toInteger . snd) $ M.toList mem

execute :: [String] -> Mask -> Mem -> Mem
execute [] mask mem = mem
execute (l : ls) mask@(m0, m1) m =
  if ("mask = " `isPrefixOf` l)
    then let newMask = parseMask l in execute ls newMask m
    else
      let (a, v) = parseMemStmt l
          newVal = masked mask v
       in execute ls mask (M.insert a newVal m)

parseMemStmt :: String -> (Int, Int)
parseMemStmt s =
  let matches = (s =~ [re|^mem\[([0-9]+)\] = ([0-9]+)$|]) :: [[String]]
   in case matches of
        [_ : addresString : valueString : []] -> (read addresString, read valueString)

masked :: Mask -> Int -> Int
masked (m0, m1) v = m0 .&. (m1 .|. v)

parseMask :: String -> Mask
parseMask str =
  -- trace (show $ length $ filter (== 'X') str) $
  let (m0, m1) = go (0, 0) 1 $ reverse $ drop 7 str
   in (complement m0, m1)
  where
    go m@(m0, m1) v [] = m
    go m@(m0, m1) v (c : cs) = case c of
      'X' -> go m (v * 2) cs
      '0' -> go (m0 .|. v, m1) (v * 2) cs
      '1' -> go (m0, m1 .|. v) (v * 2) cs

type Mask2 = (Int, Int, [Int])

runProg2 :: [String] -> Integer
runProg2 prog =
  let mem = execute2 prog (0, 0, []) M.empty
   in sum $ map (toInteger . snd) $ M.toList mem

execute2 :: [String] -> Mask2 -> Mem -> Mem
execute2 [] mask mem = mem
execute2 (l : ls) mask@(m0, m1, fs) m =
  if ("mask = " `isPrefixOf` l)
    then let newMask = parseMask2 l in execute2 ls newMask m
    else
      let (a, v) = parseMemStmt l
          base = (a .|. m1) .&. m0
          newMem = foldl' (\mem f -> M.insert (base + f) v mem) m fs
       in execute2 ls mask newMem

masked2 :: Mask -> Int -> Int
masked2 (m0, m1) v = m0 .&. (m1 .|. v)

parseMask2 :: String -> Mask2
parseMask2 str =
  -- trace (show $ length $ filter (== 'X') str) $
  let (m0, m1, l) = go (0, 0, [0]) 1 $ reverse $ drop 7 str
   in (complement m0, m1, l)
  where
    go m v [] = m
    go m@(m0, m1, l) v (c : cs) = case c of
      '0' -> go m (v * 2) cs
      '1' -> go (m0, m1 .|. v, l) (v * 2) cs
      'X' -> go (m0 .|. v, m1, l ++ (map (+ v) l)) (v * 2) cs
