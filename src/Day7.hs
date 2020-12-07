{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)

main :: IO ()
main =
  do
    strEx <- readFile "src/day7ex.txt"
    let (Right rulesEx) = parseOnly rulesFile (pack strEx)
    putStrLn $ unlines $ map show rulesEx
    print $ length rulesEx
    print $ wrappers rulesEx
    print $ countBags rulesEx "shiny gold"

    str <- readFile "src/day7.txt"
    let (Right rules) = parseOnly rulesFile (pack str)
    print $ length rules
    print $ length $ wrappers rules

    print $ countBags rules "shiny gold"

-- print $ wrappers rules

countBags :: [Rule] -> String -> Int
countBags rules color =
  let Just (Rule _ contents) = find ((color ==) . getColor) rules
      countContent (name, count) = count * (1 + countBags rules name)
   in sum $ map countContent contents

simplify :: Rule -> (String, [String])
simplify (Rule c ls) = (c, map fst ls)

wrappers :: [Rule] -> [String]
wrappers rules =
  let simpleRules = map simplify rules
      go ([], oldBags) = oldBags
      go (newBags, oldBags) =
        let bl = newBags >>= (wrapperBags simpleRules)
            bs = S.fromList bl
            nb = S.difference bs oldBags
         in go (S.toList nb, S.union oldBags nb)
   in S.toList $ go (["shiny gold"], S.empty)

wrapperBags :: [(String, [String])] -> String -> [String]
wrapperBags rules c = map fst $ filter (\(_, l) -> elem c l) rules

canPutIn :: String -> Rule -> Bool
canPutIn c (Rule color options) = c `elem` (map fst options)

data Rule = Rule
  { getColor :: String,
    getContents :: [(String, Int)]
  }
  deriving (Show)

rulesFile :: Parser [Rule]
rulesFile = many ruleParser

t0 = "plaid blue bags contain 1 dull indigo bag, 4 wavy black bags, 4 clear red bags.\n"

t1 = "plaid teal bags contain no other bags.\nplaid blue bags contain 1 dull indigo bag, 4 wavy black bags, 4 clear red bags.\n"

t2 = "plaid teal bags contain no other bags.\n"

t3 = "plaid teal bags contain no other bags.\nplaid teal bags contain no other bags.\n"

ruleParser :: Parser Rule
ruleParser = do
  color <- parseColor
  string " bags contain "
  contents <- parseContents
  return $ Rule color contents

parseContents :: Parser [(String, Int)]
parseContents =
  (string "no other bags." >> endOfLine >> return []) <|> manyTill parseContentSpec endOfLine

t :: Parser [(String, Int)]
t = many parseContentSpec

parseContentSpec :: Parser (String, Int)
parseContentSpec = do
  cnt <- decimal
  string " "
  color <- parseColor
  string (if cnt == 1 then " bag" else " bags")
  string ", " <> string "."
  return (color, cnt)

parseContentSpec1 :: Parser (String, Int)
parseContentSpec1 = do
  cnt <- decimal
  string " "
  color <- parseColor
  string (if cnt == 1 then " bag" else " bags")
  return (color, cnt)

parseColor :: Parser String
parseColor = do
  prefix <- parseToken
  string " "
  postfix <- parseToken
  return $ prefix ++ " " ++ postfix

parseToken :: Parser String
parseToken = do
  str <- Data.Attoparsec.Text.takeWhile (/= ' ')
  return (unpack str)
