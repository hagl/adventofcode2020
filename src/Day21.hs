{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text as AT
import qualified Data.HashMap.Strict as M
import Data.List (find, groupBy, intercalate, intersect, isPrefixOf, nub, partition, sort, sortOn, transpose, (\\))
import Data.List.Split
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)
import Debug.Trace
import GHC.Base (assert)
import Text.RE.TDFA.String

main :: IO ()
main = do
  solveFile "src/day21ex.txt"
  solveFile "src/day21.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let (Right items) = parseOnly programParser (pack str)
  print $ length items
  let allgs = nub $ sort $ items >>= allergens
  print allgs
  let allAllgIng = nub $ sort $ allgs >>= (possibleAllergens items)
  print allAllgIng
  print $ countSafe items allAllgIng
  solve2 items allAllgIng allgs

solve2 :: [Item] -> [String] -> [String] -> IO ()
solve2 items allAllgIng allgs = do
  let filtered = map (filterAllg allAllgIng) items
  let solved = solveMapping $ map (\a -> (a, possibleAllergens filtered a)) allgs
  print $ intercalate "," $ map snd $ sort solved

solveMapping :: [(String, [String])] -> [(String, String)]
solveMapping options = go options []
  where
    go [] acc = acc
    go list acc =
      let (found, rest) = partition (\(_, x) -> length x == 1) list
          foundMapping = map (\(k, (v : _)) -> (k, v)) found
          foundNames = map (\(k, (v : _)) -> v) found
          newOptions = map (\(k, vs) -> (k, vs \\ foundNames)) rest
       in if (null found) then acc else go newOptions (foundMapping ++ acc)

filterAllg :: [String] -> Item -> Item
filterAllg unsafe (Item ingredients allergens) = let newIngredients = filter (`elem` unsafe) ingredients in Item newIngredients allergens

countSafe :: [Item] -> [String] -> Int
countSafe items unsafe = length $ items >>= (filter (not . (`elem` unsafe)) . ingredients)

possibleAllergens :: [Item] -> String -> [String]
possibleAllergens items allg =
  let containing = map ingredients $ filter ((allg `elem`) . allergens) items
   in foldl1 intersect containing

data Item = Item {ingredients :: [String], allergens :: [String]} deriving (Show, Eq)

programParser :: Parser [Item]
programParser = itemParser `AT.sepBy` endOfLine

-- sqjhc mxmxvkd sbzzf (contains fish)

itemParser :: Parser Item
itemParser = do
  ingredients <- (many1 letter) `AT.sepBy` (string " ")
  string " (contains "
  allergens <- (many1 letter) `AT.sepBy` (string ", ")
  string ")"
  -- endOfLine
  return $ Item ingredients allergens
