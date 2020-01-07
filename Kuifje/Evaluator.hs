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

data Value = R Rational | B Bool deriving (Show, Eq, Ord)
type VEnv = E.Env Value
type Gamma = E.Env Value 

(.^) :: s -> ASetter s t a b -> b -> t
(.^) s x y = set x y s


valueToBool :: Value -> Bool
valueToBool (B b) = b

program :: Stmt -> (s ~~> s)


program :: VEnv -> Stmt -> (Kuifje Gamma, VEnv)
program ve (Seq ls) = let (g, ve') = program ve (head ls) in 
                          if length ls > 1 
                          then let (g', ve'') = program ve' (Seq (tail ls)) in (g <> g', ve'')
                          else (g, ve')

program ve (Assign id expr) = case evalExpr ve expr of 
                             (R r) -> (update (\s -> (toDist s id r)), E.add ve (id, (R r)))
                             (B b) -> (update (\s -> (toDistBool s id b)), E.add ve (id, (B b)))
program ve (Syntax.If e s1 s2) = let (B b) = evalExpr ve e
                                     (p1, ve')  = program ve s1
                                     (p2, ve'') = program ve s2 in 
                                 (cond (\s -> (uniform [b])) p1 p2, ve')
program ve (Syntax.While e s) = 
        let (p, ve') = program ve s in 
            ((while (\s -> (uniform [(valueToBool (evalExpr ve e))])) p), ve')
--(B b) = evalExpr ve e 
program ve (Syntax.Skip) = (Language.Kuifje.Syntax.Skip, ve)
program ve (Leak e) = case evalExpr E.empty e of
                     (R r) -> (observe (\s -> (uniform [r])), ve)
                     (B b) -> (observe (\s -> (uniform [b])), ve)
program ve (Vis s) = undefined
program ve (Echoice s1 s2 e) = undefined -- let (R r) = evalExpr E.empty e in
                        

-- program (Assign id expr) = update (\s -> return (s.^(id) $ evalExpr E.empty expr))
getRational :: Gamma -> String -> Rational
getRational g s | Just (R t) <- E.lookup g s = t
        | otherwise = error "Not going to happen"

toDistBool :: Gamma -> String -> Bool -> Dist Gamma
toDistBool g s x = uniform [(E.add g (s, (B x)))]

toDist :: Gamma -> String -> Rational -> Dist Gamma
toDist g s x = uniform [(E.add g (s, (R x)))]


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


example :: String
example = "x := 1; y := 0; while (x > 0) do y := x + y; x := x - 1; od;"

aProgram :: String -> VEnv
aProgram s = let (g, ve) = program E.empty (Parse.parseString s) in ve

project :: Dist (Dist Gamma) -> Dist (Dist Rational)
project = fmap (fmap (\s -> getRational s "y"))

initGamma :: Rational -> Gamma
initGamma x = let g = E.add E.empty ("x", (R x)) in 
               E.add g ("y", (R (0 % 1)))

hyper :: Dist (Dist Rational)
hyper = let (g, ve) = program E.empty (Parse.parseString example) in 
            project $ hysem g (uniform [initGamma x | x <- [5..8]])

main :: IO ()
main = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper
