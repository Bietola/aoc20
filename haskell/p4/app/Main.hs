module Main where

import Data.Function
import Data.List.Split
import qualified Data.Map as M

main :: IO ()
main = do
  inp <- getContents
  print $ inp & lines & splitWhen (=="") & fmap (fmap (splitOn ":"))
