module Party where

import Employee

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
