{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)
import Debug.Trace

main :: IO ()
main =
  do
    strEx <- readFile "src/day8ex.txt"
    let (Right instructionsEx) = parseOnly programParser (pack strEx)
    putStrLn $ unlines $ map show instructionsEx
    print $ length instructionsEx
    print $ show (run instructionsEx)
    print $ show (modify instructionsEx)

    str <- readFile "src/day8.txt"
    let (Right instructions) = parseOnly programParser (pack str)
    print $ length instructions
    print $ show (run instructions)
    print $ show (modify instructions)

run :: [Instruction] -> (Int, Bool)
run program = go 0 0 []
  where
    go ip acc visited =
      -- trace
      -- (show ip ++ " " ++ show acc)
      ( if (elem ip visited)
          then (acc, False)
          else
            if (ip >= (length program))
              then (acc, True)
              else case (program !! ip) of
                Acc da -> go (ip + 1) (acc + da) (ip : visited)
                Jmp di -> go (ip + di) (acc) (ip : visited)
                Nop _ -> go (ip + 1) (acc) (ip : visited)
      )

check :: [Instruction] -> Int -> (Int, Bool)
check program pos =
  let (start, ins : end) = splitAt pos program
   in case ins of
        Acc _ -> (0, False)
        Jmp a -> run (start ++ (Nop a) : end)
        Nop a -> run (start ++ (Jmp a) : end)

modify :: [Instruction] -> Maybe (Int, Bool)
modify program = find (\(_, f) -> f) $map (check program) [0 .. (length program)]

data Instruction
  = Nop Int
  | Jmp Int
  | Acc Int
  deriving (Show)

programParser :: Parser [Instruction]
programParser = many instructionParser

instructionParser :: Parser Instruction
instructionParser = do
  ins <- parseToken
  string " "
  sign <- (string "+" >> return 1) <|> (string "-" >> return (-1))
  num <- decimal
  endOfLine
  return $ case ins of
    "nop" -> Nop (sign * num)
    "jmp" -> Jmp (sign * num)
    "acc" -> Acc (sign * num)

parseToken :: Parser String
parseToken = do
  str <- Data.Attoparsec.Text.takeWhile (/= ' ')
  return (unpack str)
