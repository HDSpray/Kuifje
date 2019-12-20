{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module TypeChecker where

import Syntax


import Control.Applicative
import Control.Monad (void, unless)

data Type = BoolType
          | RationalType
          deriving (Show, Eq, Read)

data TypeError = TypeMismatch
                deriving (Show)

typeCheck :: Stmt -> Either TypeError Stmt
typeCheck (Seq ls) = 
  if length ls == 1 
     then typeCheck (head ls) 
     else case typeCheck (head ls) of 
            (Left e) -> Left e
            (Right stmt) -> case typeCheck (Seq (tail ls)) of
                            (Right (Seq rstmt)) -> (Right (Seq (stmt:rstmt)))
                            (Right rstmt)       -> (Right (Seq (stmt:[rstmt])))
                            e -> e
typeCheck (Assign s e) = undefined



newtype TC a = TC (Either TypeError a) deriving (Monad, Functor, Applicative, Show)

typeError :: TypeError -> TC a
typeError = TC . Left

exprCheck :: Expr -> TC Type
exprCheck (RationalConst _) = return RationalType
exprCheck (Neg e) = 
        do x <- exprCheck e 
           case x of 
             BoolType     -> return BoolType 
             RationalType -> typeError TypeMismatch
exprCheck (ABinary _ e1 e2) = 
        do e1' <- exprCheck e1 
           e2' <- exprCheck e2 
           if e1' == e2' && e1' == RationalType 
              then return RationalType 
              else typeError TypeMismatch
exprCheck (Ichoice e1 e2 e3) = 
        do e1' <- exprCheck e1 
           e2' <- exprCheck e2 
           e3' <- exprCheck e3
           if e1' == e2' && e3' == RationalType
              then return e1'
              else typeError TypeMismatch
exprCheck (BoolConst _) = return BoolType
exprCheck (Not e) = do e' <- exprCheck e 
                       if e' == BoolType 
                          then return e' 
                          else typeError TypeMismatch
exprCheck (BBinary _ e1 e2) = 
        do e1' <- exprCheck e1 
           e2' <- exprCheck e2 
           if e1' == e2' && e1' == BoolType
              then return BoolType
              else typeError TypeMismatch
exprCheck (RBinary _ e1 e2) = 
        do e1' <- exprCheck e1 
           e2' <- exprCheck e2 
           if e1' == e2' && e1' == RationalType
              then return BoolType
              else typeError TypeMismatch


        {--
exprCheck :: Expr -> Either TypeError Type
exprCheck (RationalConst ratinoal) = (Right RationalType)
exprCheck (Neg rational) = (Right RationalType)
exprCheck (ABinary op e1 e2) = (Right RationalType)
exprCheck (Ichoice e1 e2 e) = 
  case exprCheck e of
    (Left e) -> Left e
    (Right RationalType) -> 
            case exprCheck e1 of 
              (Left e) -> Left e
              (Right e1') -> case exprCheck e2 of
                               (Left e) -> Left e
                               (Right e2') -> 
                                  if e1' == e2' 
                                     then (Right e1') 
                                     else (Left (TypeMismatch e1' e2'))

--}
