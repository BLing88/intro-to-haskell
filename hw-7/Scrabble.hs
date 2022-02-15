module Scrabble where

newtype Score = Score Int

instance Semigroup Score where
  (Score m) <> (Score n) = Score (m + n)

instance Monoid Score where
  mempty = Score 0


