module Main where

preciseQuot lhs rhs =
  if lhs `mod` rhs /= 0
    then undefined
    else lhs `quot` rhs

bin2id :: String -> (Int, Int)
bin2id str = frontBack2id $ takeWhile isFrontBack str

frontBot2id :: String -> Int
frontBot2id str =
  case binPart 'F' 'B' 0 128 str of
    (x, y) | x == y -> x
    _ -> undefined

leftRight2id :: String -> (Int, Int)
leftRight2id str =
  case binPart 'L' 'R' 0 undefined str of
    (x, y) | x == y -> x
    _ -> undefined


binPart:: a -> a -> Int -> Int -> [a] -> (Int, Int)
binPart lowSym highSym initMin initMax = foldr go (initMin, initMax) . reverse
  where go frontBot (min, max) =
          let middle = (max - min) `preciseQuot` 2
            in case frontBot of
              lowSym -> (min, max - middle)
              highSym -> (min + middle, max)
              _ -> undefined

main :: IO ()
main = print "hello world"
