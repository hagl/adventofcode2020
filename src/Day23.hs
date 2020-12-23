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
  -- solve [3, 8, 9, 1, 2, 5, 4, 6, 7]
  solve [4, 1, 8, 9, 7, 6, 2, 3, 5]

solve :: [Int] -> IO ()
solve input = do
  print input
  print $ foldl' (\s _ -> step s) input [1 .. 100]
  let (p, r) = foldl' (\s _ -> step2 s) ([], input ++ [10 .. 1000000]) [1 .. 1000]
  let result = r ++ (reverse p)
  print $ take 2 result
  print $ take 3 $ dropWhile (/= 1) result

step :: [Int] -> [Int]
step (h : a : b : c : rest) =
  let n = [a, b, c]
      ip = insertionPoint (h -1) n
      insertAfter (x : xs) = if x == ip then x : a : b : c : xs ++ [h] else x : (insertAfter xs)
   in insertAfter rest
  where
    insertionPoint x l =
      let x' = if x == 0 then 9 else x
       in if (x' `elem` l) then insertionPoint (x' -1) l else x'

step2 :: ([Int], [Int]) -> ([Int], [Int])
step2 (prefix, (h : a : b : c : rest)) =
  let n = [a, b, c]
      ip = insertionPoint (h -1) n
      insertAfter p (x : xs) acc = if x == ip then (h : p, (reverse acc) ++ (a : b : c : xs)) else (insertAfter p xs (x : acc))
      insertAfter [] [] _ = error "x"
      insertAfter p [] acc = insertAfter [] (reverse p) acc
   in insertAfter prefix rest []
  where
    insertionPoint x l =
      let x' = if x == 0 then 9 else x
       in if (x' `elem` l) then insertionPoint (x' -1) l else x'
