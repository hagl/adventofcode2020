{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
  let input = [4, 1, 8, 9, 7, 6, 2, 3, 5]
  -- let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
  putStrLn $ concat $ map show $ tail $printArray 1 9 $ runST $ solve input 9 100
  print $ product $ tail $ printArray 1 3 $ runST $ solve input 1000000 10000000
  return ()

printArray :: Int -> Int -> [Int] -> [Int]
printArray start count arr = go start count []
  where
    go _ 0 acc = reverse acc
    go s c acc = go (arr !! s) (c - 1) (s : acc)

solve :: [Int] -> Int -> Int -> ST s [Int]
solve start l c = do
  arr <- newListArray (0, l) ([1 ..]) :: ST s (STUArray s Int Int)
  let next = if l > length start then length start + 1 else head start
  mapM_ (\(a, b) -> writeArray arr a b) (zip (l : start) (start ++ [next]))
  foldM_ (\i _ -> step arr l i) (head start) [1 .. c]
  elems <- getElems arr
  return $ elems

step :: (STUArray s Int Int) -> Int -> Int -> ST s Int
step arr last i = do
  x <- readArray arr i
  y <- readArray arr x
  z <- readArray arr y
  n <- readArray arr z

  writeArray arr i n
  let ip = nextIndex (i - 1) last [x, y, z]

  n1 <- readArray arr ip
  writeArray arr z n1
  writeArray arr ip x

  return n

nextIndex :: Int -> Int -> [Int] -> Int
nextIndex 0 last group = nextIndex last last group
nextIndex i last group = if i `elem` group then nextIndex (i - 1) last group else i
