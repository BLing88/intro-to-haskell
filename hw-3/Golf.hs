module Golf where

import qualified Data.Map as Map
import Data.List (foldl', transpose)

-- Exercise 1

skipN :: Int -> [a] -> [a]
skipN _ [] = []
skipN 0 as = as
skipN n as = take 1 first ++ skipN n second 
  where first = drop n as
        second = drop 1 first

skips :: [a] -> [[a]]
skips as = map (`skipN` as) [0..n - 1]
  where n = length as 

-- Exercise 2

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

-- Exercise 3

-- countMap takes a list and returns a map with the elements 
-- of the list as keys and counts of the elements as values
-- it works by first creating a new list of pairs from the old
-- list by appending 1 as the second element
-- then it constructs a map from this new list with a 
-- function to combine the values of repeat keys, i.e. addition in this case
countMap :: (Ord a) => [a] -> Map.Map a Int
countMap = Map.fromListWith (+) . flip zip (repeat 1)

getMaxCount :: (Ord a) => Map.Map a Int -> Int
getMaxCount countMap = maximum $ map snd (Map.toList countMap)

getHistogramLine :: Int -> Map.Map Int Int -> Int -> String
getHistogramLine maxCount countMap n = 
  if Map.member n countMap 
  then replicate (maxCount - count) ' '  
       ++ 
       replicate count '*' 
  else replicate maxCount ' '
  where count = countMap Map.! n

histogram :: [Int] -> String
histogram xs = (unlines . transpose $ histogramLines) ++ "==========\n0123456789"
  where maxCount = getMaxCount $ countMap xs
        histogramLines = map (getHistogramLine maxCount $ countMap xs) [0..9]
