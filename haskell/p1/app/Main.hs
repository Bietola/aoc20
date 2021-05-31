{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List

solve inp = mapM (const inp) [1 .. 2] & filter ((== 2020) . sum) & map product & nub

main = do
  inp <- getContents
  print $ solve ((read @Int) <$> lines inp)
