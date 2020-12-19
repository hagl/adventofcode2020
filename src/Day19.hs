{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text as AT
import qualified Data.HashMap.Strict as M
import Data.List (find, intercalate, isPrefixOf, sortOn, transpose)
import Data.List.Split
import qualified Data.Set as S
import Data.Text (Text, pack, strip, unpack)
import GHC.Base (assert)
import Text.RE.TDFA.String

main :: IO ()
main = do
  solveFile "src/day19ex2.txt"
  solveFile "src/day19.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let (Right (specs, text)) = parseOnly programParser (pack str)
  let simplified = resolveRules $ toMap specs
  -- putStrLn $ show $ M.map printRuleSpec $ simplified
  let regex = toRegex $ rule $ simplified M.! 0
  re <- compileRegex ("^" ++ regex ++ "$")
  print $ length $ filter (matchesRegex re) text

  let l31 = toList $ simplifyRule $ rule $ simplified M.! 31
  let l42 = toList $ simplifyRule $ rule $ simplified M.! 42

  print $ assert (S.null $ S.intersection (S.fromList l31) (S.fromList l42)) "*"

  let count = let lengths = map length (l31 ++ l42); lmin = minimum lengths in assert (lmin == maximum lengths) lmin

  -- 0: 8 11
  -- 8: 42 | 42 8
  -- 11: 42 31 | 42 11 31
  print $ length $ filter (matches0 count l42 l31) text

matches0 :: Int -> [String] -> [String] -> String -> Bool
matches0 c l42 l31 s = let grouped = chunksOf c s in go grouped (0, 0)
  where
    go [] (a, b) = a > b && b > 0
    go (x : xs) (a, 0) | x `elem` l42 = go xs (a + 1, 0)
    go (x : xs) (a, b) = if x `elem` l31 then go xs (a, b + 1) else False

matchesRegex :: RE -> String -> Bool
matchesRegex re s = matched $ s ?=~ re

toMap :: [RuleSpec] -> M.HashMap Int RuleSpec
toMap = M.fromList . map (\rule@(RuleSpec num _) -> (num, rule))

resolveRules :: (M.HashMap Int RuleSpec) -> M.HashMap Int RuleSpec
resolveRules unresolved = go M.empty unresolved
  where
    go resolved unresolved =
      let newlyResolved = filter (hasNoRef . rule) $ M.elems unresolved
          newResolved = foldr (\rule m -> M.insert (key rule) rule m) resolved newlyResolved
          remaining = foldr M.delete unresolved $ map key newlyResolved
          newUnresolved = M.map (replaceRefs newlyResolved) remaining
       in if (M.null remaining) then newResolved else go newResolved newUnresolved

simplifyRule :: Rule -> Rule
simplifyRule rule = let newRule = simplifyRule' rule in if (rule == newRule) then rule else simplifyRule newRule

simplifyRule' :: Rule -> Rule
simplifyRule' r@(Constant _) = r
simplifyRule' (Alternative rules) = simplifyAlternative $ map simplifyRule' rules
simplifyRule' (Sequence rules) = simplifySequence $ map simplifyRule' rules

simplifySequence :: [Rule] -> Rule
simplifySequence rules =
  let simplified = map Sequence $ go (reverse rules) [[]]
   in if length simplified == 1 then head simplified else Alternative simplified
  where
    go [] acc = acc
    go ((Alternative rules) : rest) acc = go rest [p : a | p <- rules, a <- acc]
    go ((Sequence rules) : rest) acc = go rest [rules ++ a | a <- acc]
    go (rule : rest) acc = go rest [rule : a | a <- acc]

simplifyAlternative :: [Rule] -> Rule
simplifyAlternative rules = Alternative $ rules >>= unfoldAlternatives

unfoldAlternatives (Alternative rules) = rules
unfoldAlternatives rule = [rule]

hasNoRef :: Rule -> Bool
hasNoRef (Constant _) = True
hasNoRef (Ref _) = False
hasNoRef (Alternative rules) = all hasNoRef rules
hasNoRef (Sequence rules) = all hasNoRef rules

replaceRefs :: [RuleSpec] -> RuleSpec -> RuleSpec
replaceRefs newResolved (RuleSpec key rule) = RuleSpec key $ foldr replaceRef rule newResolved

replaceRef :: RuleSpec -> Rule -> Rule
replaceRef (RuleSpec key replacement) = go
  where
    go rule@(Constant _) = rule
    go rule@(Ref k) = if (k == key) then replacement else rule
    go (Alternative rules) = Alternative $ map go rules
    go (Sequence rules) = Sequence $ map go rules

data Rule
  = Constant String
  | Ref Int
  | Alternative [Rule]
  | Sequence [Rule]
  deriving (Show, Eq)

printRule :: Rule -> String
printRule (Constant s) = "'" ++ s ++ "'"
printRule (Ref r) = show r
printRule (Alternative rules) = "(" ++ (intercalate " | " $ map printRule rules) ++ ")"
printRule (Sequence rules) = intercalate " " $ map printRule rules

toRegex :: Rule -> String
toRegex (Constant s) = s
toRegex (Alternative rules) = "(" ++ (intercalate "|" $ map toRegex rules) ++ ")"
toRegex (Sequence rules) = intercalate "" $ map toRegex rules

-- expect Alternative of Sequences of Constants
toList :: Rule -> [String]
toList (Alternative rules) = map concatSeq rules
  where
    concatSeq (Sequence list) = concat $ map (\(Constant s) -> s) list

data RuleSpec = RuleSpec {key :: Int, rule :: Rule} deriving (Show, Eq)

printRuleSpec :: RuleSpec -> String
printRuleSpec spec = (show $ key spec) ++ ": " ++ (printRule $ rule spec)

programParser :: Parser ([RuleSpec], [String])
programParser = do
  rules <- many ruleSpecParser
  endOfLine
  text <- takeText
  return (rules, lines $ unpack text)

ruleSpecParser :: Parser RuleSpec
ruleSpecParser = do
  num <- decimal
  string ": "
  rule <- alternativeParser
  endOfLine
  return $ RuleSpec num rule

alternativeParser :: Parser Rule
alternativeParser = do
  list <- sequnenceParser `AT.sepBy` (string " | ")
  return $ if length list == 1 then head list else Alternative list

sequnenceParser :: Parser Rule
sequnenceParser = do
  list <- elParser `AT.sepBy` (string " ")
  return $ if length list == 1 then head list else Sequence list

elParser :: Parser Rule
elParser = choice [constantParser, refParser]

constantParser :: Parser Rule
constantParser = do
  string "\""
  str <- letter
  string "\""
  return $ Constant [str]

refParser :: Parser Rule
refParser = do
  num <- decimal
  return $ Ref num
