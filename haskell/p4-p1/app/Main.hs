module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Data.List
import Data.Maybe
import Text.Printf
import Data.Function

solve2 :: String -> Int
solve2 input =
  let orderedIds = (sort . map seat2id . lines) input
    in zip [(orderedIds !! 0)..] orderedIds
       & find (\(idx, id) -> idx /= id)
       & fmap (\(idx, _) -> idx)
       & fromJust

solve1 :: String -> Int
solve1 = maximum . map seat2id . lines

seat2id :: String -> Int
seat2id syms =
  let div = fromJust $ findIndex (`elem` ['L', 'R']) syms -- simplify findIndex
      (rowSyms, colSyms) = splitAt div syms
      rowId = symbols2binary 'F' 'B' rowSyms
      colId = symbols2binary 'L' 'R' colSyms
    in rowId * 8 + colId

symbols2binary :: Char -> Char -> [Char] -> Int
symbols2binary zero one = foldl go 0
  where go acc sym =
          case sym of
            s | s == zero -> acc * 2
            s | s == one  -> acc * 2 + 1
            s -> error $ printf "Unhandled sym: %c" s

main :: IO ()
main = do
  contents <- getContents
  print $ solve1 contents
  print ""
  print $ solve2 contents
