module Solution where

import Data.List

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl' (*) 1 . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

transform :: Integer -> Integer
transform n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' n = sum $ filter even fun2Sequence
  where fun2Sequence = takeWhile (/= 1) (iterate transform n)

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

embed :: a -> Tree a
embed a = Node 0 Leaf a Leaf

heightTree :: Tree a -> Integer
heightTree Leaf = -1
heightTree (Node height _ _ _) = height

addNode :: a -> Tree a -> Tree a
addNode a Leaf = embed a
addNode a (Node height leftTree val rightTree)
  | leftHeight > rightHeight = Node height leftTree val newRightSubtree
  | rightHeight > leftHeight = Node height newLeftSubtree val rightTree
  | newLeftHeight == leftHeight = Node height newLeftSubtree val rightTree
  | newRightHeight == rightHeight = Node height leftTree val newRightSubtree
  | otherwise = Node (height + 1) newLeftSubtree val rightTree 
  where newRightSubtree = addNode a rightTree
        newRightHeight = heightTree newRightSubtree
        newLeftSubtree = addNode a leftTree
        newLeftHeight = heightTree newLeftSubtree
        leftHeight = heightTree leftTree
        rightHeight = heightTree rightTree


foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree as = foldr addNode Leaf as

isValidBalancedTree :: Tree a -> Bool
isValidBalancedTree Leaf = True
isValidBalancedTree (Node height left _ right) =  
  (height == leftHeight + 1 || height == rightHeight + 1)
  && abs (leftHeight - rightHeight) <= 1 
  && isValidBalancedTree left 
  && isValidBalancedTree right
  where leftHeight = heightTree left
        rightHeight = heightTree right

-- Exercise 3

xorFn :: Bool -> Bool -> Bool
xorFn b1 b2 = (b1 || b2) && not (b1 && b2)

xor :: [Bool] -> Bool
xor [] = True
xor [b] = b
xor bs = foldr1 xorFn bs
