module Main where

preciseQuot lhs rhs =
  if lhs `mod` rhs /= 0
    then undefined
    else lhs `quot` rhs

frontBot2id :: String -> (Int, Int)
frontBot2id = foldr go (0, 128) . reverse
  where go frontBot (min, max) =
          let middle = (max - min) `preciseQuot` 2
            in case frontBot of
              'B' -> (min + middle, max)
              'F' -> (min, max - middle)
              _ -> undefined

main :: IO ()
main = print "hello world"
