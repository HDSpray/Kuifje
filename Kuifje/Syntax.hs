{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module Syntax where

data BBinOp = And 
            | Or 
            deriving (Show)

data RBinOp = Gt 
            | Ge
            | Lt
            | Le
            | Eq
            | Ne
            deriving (Show)

data Expr a where
  Var :: String -> Expr a
  RationalConst :: Rational -> Expr Rational
  Neg :: Expr Rational -> Expr Rational
  ABinary :: ABinOp -> Expr Bool -> Expr Bool -> Expr Bool
  Ichoice :: Expr a -> Expr a -> Expr Rational -> Expr a
  BoolConst :: Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  BBinary :: BBinOp -> Expr Bool -> Expr Bool -> Expr Bool
  RBinary :: RBinOp -> Expr Rational -> Expr Rational -> Expr Bool
deriving instance Show a => Show (Expr a)


-- evalExpr :: Expr a -> (s -> D a)
-- evalExpr = undefined


data ABinOp = Add 
            | Subtract 
            | Multiply 
            | Divide 
            -- | Rem
            deriving (Show)

data Stmt = 
        Seq [Stmt] 
          | forall a. (Show a) => Assign String (Expr a)
          | If (Expr Bool) Stmt Stmt 
          | While (Expr Bool) Stmt 
          | Skip 
          | forall a. (Show a) => Leak (Expr a)
          | Vis String
          | Echoice Stmt Stmt (Expr Rational)-- (Stmt Stmt Prob)
deriving instance Show Stmt
