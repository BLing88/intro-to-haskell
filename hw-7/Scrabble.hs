module Scrabble where

import Data.Char (toLower)

newtype Score = Score Int 
  deriving (Eq, Show)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
  (Score m) <> (Score n) = Score (m + n)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c = case toLower c of
  'a' -> Score 1
  'b' -> Score 3 
  'c' -> Score 3
  'd' -> Score 2
  'e' -> Score 1
  'f' -> Score 4
  'g' -> Score 2
  'h' -> Score 4
  'i' -> Score 1
  'j' -> Score 8
  'k' -> Score 5
  'l' -> Score 1
  'm' -> Score 3
  'n' -> Score 1
  'o' -> Score 1
  'p' -> Score 3
  'q' -> Score 10
  'r' -> Score 1
  's' -> Score 1
  't' -> Score 1
  'u' -> Score 1
  'v' -> Score 4
  'w' -> Score 4
  'x' -> Score 8
  'y' -> Score 4
  'z' -> Score 10
  _ -> Score 0

scoreString :: String -> Score
scoreString s = mconcat $ map score s
