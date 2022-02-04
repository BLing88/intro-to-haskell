module Golf where

import qualified Data.Map as Map

skipN :: Int -> [a] -> [a]
skipN _ [] = []
skipN 0 as = as
skipN n as = take 1 first ++ skipN n second 
  where first = drop n as
        second = drop 1 first

skips :: [a] -> [[a]]
skips as = map (`skipN` as) [0..n - 1]
  where n = length as 

isLocalMaximum :: (Ord a) => (a, a, a) -> Bool
isLocalMaximum (left, mid, right) = mid > left && mid > right

-- windows takes a list of integers and returns a list of tuples
-- that represent the sliding windows to check for local maxima
windows :: [a] -> [(a, a, a)]
windows [] = []
windows [_] = []
windows [_, _] = []
windows xs = (left, mid, right) : windows (drop 1 xs)
  where [left, mid, right] = take 3 xs

-- localMaxima first filters out non local maxima windows and then 
-- extracts the maximum from each window
localMaxima :: (Ord a) => [a] -> [a]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima xs = map (\(_, mid, _) -> mid) $ filter isLocalMaximum (windows xs)

