module Golf where

import Data.List

skipN :: Int -> [a] -> [a]
skipN _ [] = []
skipN 0 as = as
skipN n as = take 1 first ++ skipN n second 
  where first = drop n as
        second = drop 1 first

skips :: [a] -> [[a]]
skips as = map (`skipN` as) [0..n - 1]
  where n = length as 

isLocalMaximum :: (Integer, Integer, Integer) -> Bool
isLocalMaximum (left, mid, right) = mid > left && mid > right

-- windows takes a list of integers and returns a list of tuples
-- that represent the sliding windows to check for local maxima
windows :: [Integer] -> [(Integer, Integer, Integer)]
windows [] = []
windows [_] = []
windows [_, _] = []
windows xs = (left, mid, right) : windows (drop 1 xs)
  where [left, mid, right] = take 3 xs

-- localMaxima first filters out non local maxima windows and then 
-- extracts the maximum from each window
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima xs = map (\(left, mid, right) -> mid) $ filter isLocalMaximum (windows xs)
