{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where
--import ExprT
import Parser
import StackVM 

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

--instance Expr ExprT where
--  lit = Lit
--  add = Add
--  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit b
    | b <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

--eval :: ExprT -> Integer
--eval (Lit n) = n
--eval (Add l r) = eval l + eval r
--eval (Mul l r) = eval l * eval r

--evalStr :: String -> Maybe Integer
--evalStr s =
--  case parseExp Lit Add Mul s of
--    Just expr -> Just (eval expr)
--    Nothing -> Nothing

--reify :: ExprT -> ExprT
--reify = id


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7  = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 (mod n 7)
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ( mod (a*b) 7)


instance Expr Program where
  lit i = [PushI i]
  add x y = (x ++ y) ++ [Add]
  mul x y = (x ++ y) ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


runv :: Maybe Program -> Either String StackVal
runv (Just p) = stackVM p
runv _ = Left "woops"
