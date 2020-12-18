{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text as AT
import qualified Data.HashMap.Strict as M
import Data.Ix
import Data.List (find, isPrefixOf, sortOn, transpose)
import Data.List.Split
import Data.Text (Text, pack, strip, unpack)

main :: IO ()
main = do
  solveFile "src/day18ex.txt"
  solveFile "src/day18.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let (Right expressions) = parseOnly programParser (pack str)
  -- print $ expressions
  -- print $ map evaluate expressions
  print $ sum $ map evaluate expressions

evaluate :: Expression -> Int
evaluate (Constant n) = n
evaluate (Add l r) = (evaluate l) + (evaluate r)
evaluate (Mult l r) = (evaluate l) * (evaluate r)

data Expression
  = Constant Int
  | Add Expression Expression
  | Mult Expression Expression
  deriving (Show, Eq)

programParser :: Parser ([Expression])
programParser = many expressionParser

expressionParser :: Parser Expression
expressionParser = do
  left <- constantParser <|> paranthesisParser
  rest <- restParser left
  return rest

restParser :: Expression -> Parser Expression
restParser e = option e $ choice [restParser' e, fmap (\_ -> e) endOfLine]

restParser' :: Expression -> Parser Expression
restParser' e = do
  string " "
  op <- choice [string "+", string "*"]
  string " "
  right <- constantParser <|> paranthesisParser
  let exp =
        ( case unpack op of
            "+" -> Add e right
            "*" -> Mult e right
        )
  rest <- restParser exp
  return $ rest

constantParser :: Parser Expression
constantParser = do
  d <- decimal
  return $ Constant d

paranthesisParser :: Parser Expression
paranthesisParser = do
  string "("
  ex <- expressionParser
  string ")"
  return ex
