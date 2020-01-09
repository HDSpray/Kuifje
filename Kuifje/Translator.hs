{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Translator where 

import qualified Env as E
import Parse
import Syntax

import Prelude hiding ((!!), return, fmap, (>>=))
import Control.Lens hiding (Profunctor)
import Data.Semigroup
import Data.Ratio
import Data.Map.Strict
import Data.List

import Language.Kuifje.Distribution
import PrettyPrint 
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax

import System.IO 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

-- data Value = R Rational | B Bool deriving (Show, Eq, Ord)
-- type VEnv = E.Env Value
-- type Gamma = E.Env Value 

aOperator :: (Rational -> Rational -> Rational) -> 
        Dist (Either Bool Rational) -> 
        Dist (Either Bool Rational) -> 
        Dist (Either Bool Rational)
aOperator op d1 d2 = 
  D $ fromListWith (+) [((Right (op x y)), p * q) | (Right x, p) <- toList $ runD d1,
                                                    (Right y, q) <- toList $ runD d2]
cOperator op d1 d2 = 
  D $ fromListWith (+) [((Left (op x y)), p * q) | (Right x, p) <- toList $ runD d1,
                                                   (Right y, q) <- toList $ runD d2]
bOperator op d1 d2 = 
  D $ fromListWith (+) [((Left (op x y)), p * q) | (Left x, p) <- toList $ runD d1,
                                                   (Left y, q) <- toList $ runD d2]


evalE :: Expr -> (Gamma ~> (Either Bool Rational))
evalE (Var id) = \s -> case E.lookup s id of 
                          Just (R r) -> (return (Right r))
                          Just (B b) -> (return (Left b))
evalE (RationalConst r) = \s -> return (Right r)
evalE (Neg r) = \s -> 
        let r' = (evalE r) s in 
            (fmap (\p -> case p of (Right p') -> Right (-1 * p'))) r'
evalE (ABinary op e1 e2) = \s -> 
  let e1' = (evalE e1) s
      e2' = (evalE e2) s 
   in case op of 
        Add      -> (aOperator (+) e1' e2')
        Subtract -> (aOperator (-) e1' e2')
        Multiply -> (aOperator (*) e1' e2')
        Divide   -> (aOperator (/) e1' e2')
evalE (Ichoice e1 e2 p) = \s -> 
  let e1' = (evalE e1) s
      e2' = (evalE e2) s 
      p'  = Data.List.foldr (\x y -> case x of (Right x', q) -> x'*q*y) 1 
              $ toList $ runD $ (evalE p ) s
      d1 = D $ Data.Map.Strict.map (*p') $ runD e1'
      d2 = D $ Data.Map.Strict.map (*(1-p')) $ runD e2'
   in D $ unionWith (+) (runD d1) (runD d2)
evalE (BoolConst b) = \s -> return (Left b)
evalE (Not b) = \s -> 
        let r' = (evalE b) s 
         in (fmap (\bv -> case bv of 
                            (Left b') -> Left (not b'))) r'
evalE (BBinary op e1 e2) = \s -> 
  let e1' = (evalE e1) s
      e2' = (evalE e2) s in 
      case op of 
        And ->      (bOperator (&&) e1' e2')
        Or  ->      (bOperator (||) e1' e2')
evalE (RBinary op e1 e2) = \s -> 
  let e1' = (evalE e1) s
      e2' = (evalE e2) s in 
      case op of 
        Gt -> (cOperator (>) e1' e2')
        Ge -> (cOperator (>=) e1' e2')
        Lt -> (cOperator (<) e1' e2')
        Le -> (cOperator (<=) e1' e2')
        Eq -> (cOperator (==) e1' e2')
        Ne -> (cOperator (/=) e1' e2')


translateKuifje :: Stmt -> Kuifje Gamma 
translateKuifje (Seq []) = skip
translateKuifje (Seq ls) = translateKuifje (head ls) <> translateKuifje (Seq (tail ls))
translateKuifje (Assign id expr) = Language.Kuifje.Syntax.update (\s -> 
        let currS = (evalE expr) s in
            fmap (\r -> case r of 
                          (Right r) -> E.add s (id, R r)
                          (Left b)  -> E.add s (id, B b)) currS)
translateKuifje (Syntax.While e s) = 
        Language.Kuifje.Syntax.while (\s -> 
                let currS = (evalE e) s in 
                    fmap (\r -> case r of (Left b) -> b) currS) (translateKuifje s)
translateKuifje (Syntax.If e s1 s2) = 
        Language.Kuifje.Syntax.cond 
          (\s -> let currS = (evalE e) s in fmap (\r -> case r of (Left b) -> b) currS) 
          (translateKuifje s1) 
          (translateKuifje s2)
translateKuifje Syntax.Skip = skip
translateKuifje (Leak e) = observe (evalE e)
translateKuifje (Vis s) = undefined
translateKuifje (Echoice s1 s2 p) = 
        Language.Kuifje.Syntax.cond 
          (\s -> let p' = (evalE (Ichoice (BoolConst True) (BoolConst False) p) s) 
                  in (fmap (\r -> case r of (Left b) -> b)) p') 
          (translateKuifje s1) 
          (translateKuifje s2)

getRational :: Gamma -> String -> Rational
getRational g s | Just (R t) <- E.lookup g s = t
                | otherwise = error ("Not going to happen " ++ s)

project :: Dist (Dist Gamma) -> Dist (Dist Rational)
project = fmap (fmap (\s -> getRational s "y"))

initGamma :: Rational -> Rational -> Gamma
initGamma x y = let g = E.add E.empty ("x", (R x)) in 
               E.add g ("y", (R y))

hyper :: Dist (Dist Rational)
hyper = let g = translateKuifje exampelS --(Parse.parseString example)
         in project $ hysem g (uniform [E.empty])--(uniform [initGamma x y | x <- [0..1], y <- [1..2]])

example :: String
example = "y := 0; while (x > 0) do y := x + y; x := x - 1; od;"

exa :: Expr
exa = (RationalConst (5%1))
example1 = let (p) = evalE exa in p E.empty
-- example = "z := x + y; leak x;"

exampelS :: Stmt
exampelS = let (Seq ls) = parseString example 
            in Seq $ (Assign "x" (Ichoice
                        (RationalConst (1 % 1)) 
                        (RationalConst (2 % 1)) 
                        (RationalConst (1 % 2)) )):ls
main :: IO ()
main = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper
