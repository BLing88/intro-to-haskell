{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser (parseExp)
import qualified StackVM as SVM
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s 

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a 
  add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n = n > 0 
  add b1 b2 = b1 || b2
  mul b1 b2 = b1 && b2

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax 
  add (MinMax m) (MinMax n) = MinMax (max m n)
  mul (MinMax m) (MinMax n) = MinMax (min m n)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 m) (Mod7 n) = Mod7 $ (m `mod` 7) + (n `mod` 7)
  mul (Mod7 m) (Mod7 n) = Mod7 $ ((m `mod` 7) * (n `mod` 7)) `mod` 7

testExpr :: Expr a => Maybe a
testExpr = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExpr :: Maybe Integer
testBool = testExpr :: Maybe Bool
testMinMax = testExpr :: Maybe MinMax
testMod7 = testExpr :: Maybe Mod7

-- Exercise 5
instance Expr SVM.Program where
  lit n = [SVM.PushI n]
  add p1 p2 = case (result1, result2) of
    (Right (SVM.IVal m), Right (SVM.IVal n)) -> [SVM.PushI $ add m n]
    (_, _) -> []
    where result1 = SVM.stackVM p1
          result2 = SVM.stackVM p2
  mul p1 p2 =  case (result1, result2) of
    (Right (SVM.IVal m), Right (SVM.IVal n)) -> [SVM.PushI $ mul m n]
    (_, _) -> []
    where result1 = SVM.stackVM p1
          result2 = SVM.stackVM p2

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

x :: Either String SVM.StackVal
x = SVM.stackVM (add (lit 4) (mul (lit (-9)) (lit (-2)))) -- Right (IVal 22)

expr :: Expr a => a
expr = mul (add (lit 4) (mul (lit (-1)) (lit 3))) (mul (lit 5) (lit 8))

test = SVM.stackVM expr -- Right (SVM.IVal 40))

-- Exercisse 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  -- map has the mapping from variables to values
  -- and the expressions take the map and retrieve
  -- the value, if any
  add expression1 expression2 map = (+) <$> expression1 map <*> expression2 map
  mul expression1 expression2 map = (*) <$> expression1 map <*> expression2 map

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


