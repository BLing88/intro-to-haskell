module Fibonacci where

import Data.List (unfoldr)

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = unfoldr (\(a,b) -> Just (a, (b, a + b))) (0, 1)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a aStream) = a : streamToList aStream

instance Show a => Show (Stream a) where
  show stream = show $ take 20 (streamToList stream)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a aStream) = Cons (f a) (streamMap f aStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

