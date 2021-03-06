{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

parse =
  lines >>>
  splitWhen (== "") >>>
  map (M.fromList . map (intoTuple . splitWhen (== ':')) . words . unwords)
  where
    intoTuple = \[x, y] -> (x, y)

main :: IO ()
main = do
  inp <- getContents
  print $ parse inp
