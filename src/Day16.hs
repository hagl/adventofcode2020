{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text as AT
import Data.Ix
import Data.List (find, isPrefixOf, sortOn, transpose)
import Data.Text (Text, pack, strip, unpack)

main :: IO ()
main = do
  solveFile "src/day16ex.txt"
  solveFile "src/day16.txt"

data Rule = Rule {name :: String, range1 :: (Int, Int), range2 :: (Int, Int)} deriving (Show)

type Ticket = [Int]

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let (Right (rules, myTicket, otherTickets)) = parseOnly programParser (pack str)
  let result = sumInvalidValues rules otherTickets
  print result
  let validTickets = myTicket : (removeInvalidTickets rules otherTickets)
  let order = solveOptions $ getOptions rules validTickets
  print $ product $ map fst $ filter (\(_, Rule name _ _) -> isPrefixOf "departure" name) $ zip myTicket $ map (\x -> rules !! x) order

-- list of valid rules per column -> selected rules
solveOptions :: [[Int]] -> [Int]
solveOptions columnRules = map (head . fst) $ sortOn snd $ go (zip columnRules [0 ..]) []
  where
    go [] acc = acc
    go list acc =
      let Just picked@([no], _) = find (\(rules, _) -> length rules == 1) list
          rest = filter (/= picked) list
          removed = map (\(rules, ix) -> (filter (/= no) rules, ix)) rest
       in go removed (picked : acc)

-- returns for each positions which rules are compatible:
getOptions :: [Rule] -> [Ticket] -> [[Int]]
getOptions rules tickets =
  let columns = transpose tickets
      indexedRules = zip rules [0 ..]
   in map (compatibleRules indexedRules) columns

compatibleRules :: [(Rule, Int)] -> [Int] -> [Int]
compatibleRules indexedRules values = map snd $ filter (\(rule, _) -> all (matchRule rule) values) indexedRules

matchRule :: Rule -> Int -> Bool
matchRule r n = inRange (range1 r) n || inRange (range2 r) n

sumInvalidValues :: [Rule] -> [Ticket] -> Int
sumInvalidValues rules tickets = sum $ filter (dontMatchAnyRule rules) $ concat tickets

removeInvalidTickets :: [Rule] -> [Ticket] -> [Ticket]
removeInvalidTickets rules tickets = filter (not . any (dontMatchAnyRule rules)) tickets

dontMatchAnyRule :: [Rule] -> Int -> Bool
dontMatchAnyRule rules n = not $ any (flip matchRule n) rules

programParser :: Parser ([Rule], Ticket, [Ticket])
programParser = do
  rules <- many instructionParser
  endOfLine
  string "your ticket:"
  endOfLine
  myTicket <- ticketParser
  endOfLine
  string "nearby tickets:"
  endOfLine
  otherTickets <- many ticketParser
  return (rules, myTicket, otherTickets)

instructionParser :: Parser Rule
instructionParser = do
  name <- AT.takeWhile (/= ':')
  string ": "
  range1 <- parseRange
  string " or "
  range2 <- parseRange
  endOfLine
  return $ Rule (unpack name) range1 range2

parseRange :: Parser (Int, Int)
parseRange = do
  left <- decimal
  string "-"
  right <- decimal
  return (left, right)

ticketParser :: Parser Ticket
ticketParser = do
  list <- decimal `sepBy` (char ',')
  endOfLine
  return list