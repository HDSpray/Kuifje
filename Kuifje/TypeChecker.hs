{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module TypeChecker where

import qualified Env as E
import Syntax


import Control.Applicative
import Control.Monad (void, unless)

data Type = BoolType
          | RationalType
          deriving (Show, Eq, Read)

data TypeError = TypeMismatch
               | NoSuchVariable String
               | DifferentType
                deriving (Show)

type Gamma = E.Env Type

initialGamma :: Gamma
initialGamma = E.empty

newtype TC a = TC (Either TypeError a) deriving (Monad, Functor, Applicative, Show)

typeError :: TypeError -> TC a
typeError = TC . Left

typeCheck :: Gamma -> Stmt -> TC (Gamma, Stmt) -- Either TypeError Stmt
typeCheck g (Seq []) = return (g, (Seq []))
typeCheck g (Seq ls) = 
        do (g', e)  <- typeCheck g (head ls) 
           (g'', e) <- typeCheck g' (Seq (tail ls)) 
           return (g', (Seq ls))
typeCheck g (Assign s e) 
  | Just t <- E.lookup g s = 
          case exprCheck e of 
            (TC (Left err)) -> typeError err
            (TC (Right ty)) -> if t == ty 
                                  then return (g, (Assign s e)) 
                                  else typeError TypeMismatch
  | otherwise = 
          case exprCheck e of 
            (TC (Left err)) -> typeError err
            (TC (Right ty)) -> let g' = E.add g (s, ty) in 
                                   return (g', (Assign s e))
typeCheck g (If e stmt1 stmt2) = 
        case exprCheck e of 
          (TC (Left err)) -> typeError err
          (TC (Right RationalType)) -> typeError TypeMismatch
          (TC (Right BoolType)) -> do (g1, stmt1') <- typeCheck g stmt1
                                      (g2, stmt2') <- typeCheck g stmt2
                                      return (g1, (If e stmt1 stmt2))
typeCheck g (While e stmt) = 
        case exprCheck e of 
          (TC (Left err)) -> typeError err
          (TC (Right RationalType)) -> typeError TypeMismatch
          (TC (Right BoolType)) -> do (g', stmt) <- typeCheck g stmt 
                                      return (g', (While e stmt))
typeCheck g Skip = return (g, Skip)
typeCheck g (Leak e) = 
        case exprCheck e of
          (TC (Right _)) -> return (g, (Leak e))
          (TC (Left er)) -> typeError er
typeCheck g (Vis s) = return (g, (Vis s))
typeCheck g (Echoice stmt1 stmt2 e) = 
        case exprCheck e of 
          (TC (Right BoolType)) -> typeError TypeMismatch
          (TC (Left er)) -> typeError er
          (TC (Right RationalType)) -> 
                  do (g1, stmt1') <- typeCheck g stmt1
                     (g2, stmt2') <- typeCheck g stmt2
                     if g1 == g2 
                        then return (g1, (Echoice stmt1 stmt2 e)) 
                        else typeError TypeMismatch


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
