{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


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

data ExprTy = EBool | ERational
        deriving (Show)

        {-
data Expr (a :: ExprTy) where
  Var :: String -> Expr a
  RationalConst :: Rational -> Expr ERational
  Neg :: Expr ERational -> Expr ERational
  ABinary :: ABinOp -> Expr EBool -> Expr EBool -> Expr EBool
  Ichoice :: Expr a -> Expr a -> Expr ERational -> Expr a
  BoolConst :: Bool -> Expr EBool
  Not :: Expr EBool -> Expr EBool
  BBinary :: BBinOp -> Expr EBool -> Expr EBool -> Expr EBool
  RBinary :: RBinOp -> Expr ERational -> Expr ERational -> Expr EBool
deriving instance Show (Expr a)


-- evalExpr :: Expr a -> (s -> D a)
-- evalExpr = undefined
-- -}



data Expr = Var String 
          | RationalConst Rational
          | Neg Expr 
          | ABinary ABinOp Expr Expr 
          | Ichoice Expr Expr Expr -- (Expr Expr Prob)

          -- Bool Expr
          | BoolConst Bool
          | Not Expr 
          | BBinary BBinOp Expr Expr 
          | RBinary RBinOp Expr Expr 
          deriving (Show)


data ABinOp = Add 
            | Subtract 
            | Multiply 
            | Divide 
            -- | Rem
            deriving (Show)

data Stmt = Seq [Stmt] 
          | Assign String Expr
          | If Expr Stmt Stmt 
          | While Expr Stmt 
          | Skip 
          | Leak Expr
          | Vis String
          | Echoice Stmt Stmt Expr 
deriving instance Show Stmt
