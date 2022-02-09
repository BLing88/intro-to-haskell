module Calc where

import ExprT
import Parser (parseExp)

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s 
