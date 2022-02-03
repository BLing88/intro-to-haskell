module Checksum where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = a : toDigitsRev b
    where 
      a = n `mod` 10
      b = n `div` 10

toDigits :: Integer -> [Integer]
toDigits n 
  | n <= 0 = []
  | otherwise = reverse $ toDigitsRev n

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [a] = [a]
doubleEveryOtherFromLeft (a : b : xs) = a : (2 * b) : doubleEveryOtherFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [n]  
  | n < 10 = n
  | otherwise = sumDigits (toDigits n)
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = 
  (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

-- move n a b returns a list of moves 
-- to move n pegs from peg start to peg end
-- using peg temp as temporary storage
move :: Integer -> Peg -> Peg -> Peg -> [Move]
move n start end temp
  | n <= 0 = []
  | otherwise = move (n - 1) temp end start ++ (start, end) : move (n - 1) start temp end  

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end temp = reverse $ move n start end temp

main :: IO ()
main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882
  print $ hanoi 2 "a" "b" "c"
  print $ hanoi 3 "a" "b" "c"
