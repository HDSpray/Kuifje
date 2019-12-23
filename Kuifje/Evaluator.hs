{-# LANGUAGE TemplateHaskell #-}
module Evaluator where
import qualified Env as E

import Parse
import Syntax

import Prelude hiding ((!!), return, fmap)
import Control.Lens hiding (Profunctor)
import Data.Semigroup
import Data.Ratio

import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint ()
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax

import System.IO 
-- import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Value = R Rational | B Bool deriving (Show)
type VEnv = E.Env Value

(.^) :: s -> ASetter s t a b -> b -> t
(.^) s x y = set x y s


parseString :: String -> Stmt
parseString str =
        case parse whileParser "" str of
          Left e  -> error $ show e
          Right r -> r


program :: Stmt -> Kuifje a
program (Seq ls) = if length ls > 1 
                    then program (head ls) <> program (Seq (tail ls))
                    else program (head ls)
-- program (Assign id expr) = update (\s -> return (s.^id $ evalExpr E.empty expr))

evalExpr :: VEnv -> Expr -> Value
evalExpr e (Var s) | Just t <- E.lookup e s = t
                   | otherwise = error "No such variable"
evalExpr e (RationalConst r) = R r
evalExpr e (Neg expr) = let R value = evalExpr e expr in R (-1*value)
evalExpr e (ABinary op expr1 expr2) = 
        let R value1 = evalExpr e expr1
            R value2 = evalExpr e expr2 in
            case op of 
              Add      -> R (value1 + value2)
              Subtract -> R (value1 - value2)
              Multiply -> R (value1 * value2)
              Divide   -> R (value1 / value2)
{--
evalExpr e (Ichoice expr1 expr2 expr3) = 
        let R value1 = evalExpr e expr1
            R value2 = evalExpr e expr2
            R value3 = evalExpr e expr3 in
            R value1
--}
evalExpr e (BoolConst b) = B b
evalExpr e (Not expr) = let B value = evalExpr e expr in B (not value)
evalExpr e (BBinary op expr1 expr2) = 
        let B value1 = evalExpr e expr1
            B value2 = evalExpr e expr2 in
            case op of 
              And -> B (value1 && value2)
              Or  -> B (value1 || value2)
evalExpr e (RBinary op expr1 expr2) = 
        let R value1 = evalExpr e expr1
            R value2 = evalExpr e expr2 in
            case op of 
              Gt -> B (value1 > value2)
              Ge -> B (value1 >= value2)
              Lt -> B (value1 < value2)
              Le -> B (value1 <= value2)
              Eq -> B (value1 == value2)
              Ne -> B (value1 /= value2)
