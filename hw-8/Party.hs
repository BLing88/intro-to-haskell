module Party where

import Employee
import Data.Tree (Tree(Node, rootLabel, subForest))

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList 
-- assumes boss or any of their subordinates is in the list already
glCons emp@Emp { empFun = fun } (GL list listFun) = GL (emp : list) (fun + listFun)


instance Semigroup GuestList where
  -- assumes disjoint lists
  (GL list1 fun1) <> (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2) 

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun glist1@(GL _ fun1) glist2@(GL _ fun2) 
  | fun1 >= fun2 = glist1
  | otherwise = glist2

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = root, subForest = subforest } = 
  f root (map (treeFold f) subforest)


test :: Employee -> [Fun] -> Fun
test (Emp _ fun) funs = fun + sum funs

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp@Emp { empFun = fun } [] =  (GL [emp] fun, GL [] 0)
nextLevel boss glists = (glCons boss withBoss, withoutBoss)
  where (withBoss, withoutBoss) = mconcat $ map (\(with, without) -> (without, moreFun with without)) glists 

-- Exercise 4
maxFun :: Tree Employee -> GuestList 
maxFun company = moreFun withBoss withoutBoss
  where (withBoss, withoutBoss) = treeFold nextLevel company
