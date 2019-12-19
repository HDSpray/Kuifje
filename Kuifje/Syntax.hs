module Syntax where

data BExpr = BoolConst Bool
           | Not BExpr 
           | BBinary BBinOp BExpr BExpr 
           | RBinary RBinOp AExpr AExpr 
           deriving (Show)

data BBinOp = And 
            | Or 
            deriving (Show)

data RBinOp = Gt
            | Ge
            | Lt
            | Le
            | Eq
            deriving (Show)

data AExpr = Var String 

           | RationalConst Rational

           
           | Neg AExpr 
           | ABinary ABinOp AExpr AExpr 
           | Ichoice AExpr AExpr AExpr
           deriving (Show)

data ABinOp = Add 
            | Subtract 
            | Multiply 
            | Divide 
            deriving (Show)

data Stmt = Seq [Stmt] 
          | Assign String AExpr 
          | If BExpr Stmt Stmt 
          | While BExpr Stmt 
          | Skip 
          | Leak AExpr
          | Vis String
          | Echoice Stmt Stmt AExpr
          deriving (Show)

