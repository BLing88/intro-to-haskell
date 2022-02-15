module JoinList where

import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:xs) !!? 0     = Just x
(x:xs) !!? i     = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ jl = jl
jl +++ Empty = jl
j1 +++ j2 = Append (tag j1 <> tag j2) j1 j2
 

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

-- Exercise 2
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append m left right) 
  | n > totalSize - 1 || n < 0 = Nothing
  | n <= leftSize - 1 = indexJ n left
  | otherwise = indexJ (n - leftSize) right
  where totalSize = getSize $ size m
        leftSize = getSize $ size (tag left)

jl :: JoinList Size Int
jl = Append (Size 6) 
  (Append (Size 4) 
    (Append 2 (Single 1 4) (Single 1 5))
    (Append 2 (Single 1 6) (Single 1 9))) 
  (Append (Size 2) 
    (Single 1 17) 
    (Single 1 0)) 

dropJ :: (Sized b, Monoid b) => 
         Int -> JoinList b a -> JoinList b a
dropJ n jl | n >= getSize (size (tag jl)) = Empty
           | n <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ left right) 
  | n <= leftSize = newLeft +++ right
  | otherwise = dropJ (n - leftSize) right
  where leftSize = getSize $ size (tag left)
        newLeft = dropJ n left

takeJ :: (Sized b, Monoid b) => 
         Int -> JoinList b a -> JoinList b a
takeJ n jl | n <= 0 = Empty
           | n >= getSize (size (tag jl)) = jl
takeJ _ Empty = Empty
takeJ _ (Single m a) = Single m a
takeJ n (Append m left right)
  | n <= leftSize = takeJ n left
  | otherwise = left +++ takeJ (n - leftSize) right
  where leftSize = getSize $ size (tag left)

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
