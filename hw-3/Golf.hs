module Golf where

skipN :: Int -> [a] -> [a]
skipN _ [] = []
skipN 0 as = as
skipN n as = take 1 first ++ skipN n second 
  where first = drop n as
        second = drop 1 first

skips :: [a] -> [[a]]
skips as = map (`skipN` as) [0..n - 1]
  where n = length as 

