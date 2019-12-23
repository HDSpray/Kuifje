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
          | Echoice Stmt Stmt Expr -- (Stmt Stmt Prob)
          deriving (Show)

