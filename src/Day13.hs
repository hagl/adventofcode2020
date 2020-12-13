module Main where

import Data.List

main :: IO ()
main =
  do
    solveFile "src/day13ex.txt"
    solveFile "src/day13.txt"

solveFile :: String -> IO ()
solveFile fileName = do
  str <- readFile fileName
  let [timeStr, busListStr] = lines str
  let time = read timeStr :: Int
  let busses = (map read $ filter (/= "x") $ splitList ',' busListStr) :: [Int]
  let bus = head $ sortOn (\x -> x - (mod time x)) busses
  print $ bus * (bus - (mod time bus))
  let busTime = map (\(x, d) -> let m = read x in (m, (m - d) `mod` m)) $ filter ((/= "x") . fst) $ (zip (splitList ',' busListStr) [0 ..]) :: [(Integer, Integer)]
  print $ foldl' go (0, 1) busTime

go :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
go (first, step) (m, d) =
  let fm = first `mod` m
      sm = step `mod` m
      Just sm1 = find (\x -> (fm + x * sm) `mod` m == d) [1 ..]
      Just sm2 = find (\x -> (x * sm) `mod` m == 0) [1 ..]
   in (first + sm1 * step, sm2 * step)

splitList :: Eq a => a -> [a] -> [[a]]
splitList a as = reverse $ map reverse $ splitList' as [[]]
  where
    splitList' [] acc = acc
    splitList' (b : bs) acc@(c : cs) =
      if a == b then splitList' bs ([] : acc) else splitList' bs ((b : c) : cs)
