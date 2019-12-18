module Kuifje.Syntax where

import Data.List

type Id = String
type Program = [Bind]

data Stmt
    = Var Id 
    | Skip
    | Seq Stmt Stmt
    | Assign Id Expr
    | WHILE Expr Stmt
    | IF Expr Stmt Stmt
    | LEAK Expr
    | HIO Id
    | VIS Id


data Expr
    = Var Id
    | Num Integer
    | Bool Bool

    | App Expr Expr
    | WHILE Expr Expr
    | IF Expr Expr Expr
    | LEAK Expr
    | IChoice Expr Int Expr
    | EChoice Expr Int Expr

data Bind = Bind Id Type [Id] Exp
  deriving (Read, Show, Eq)

data Op = Add
	| Sub
 	| Mul
 	| Quot
	| Rem
	| Neg
	| Gt
 	| Ge
	| Lt
	| Le
	| Eq
	| Ne
	deriving (Show, Eq, Read)

