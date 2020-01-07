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

singlePoint = uniform [1 % 1]


ariOperate :: (Rational -> Rational -> Rational) -> 
        Dist Rational -> Dist Rational -> Dist Rational
ariOperate op d1 d2 = 
        D $ fromListWith (+) [((op x y), p * q) | (x, p) <- toList $ runD d1,
                                                 (y, q) <- toList $ runD d2]
rCompare :: (Rational -> Rational -> Bool) -> Dist Rational -> Dist Rational -> 
        Dist Bool
rCompare op d1 d2 = 
        D $ fromListWith (+) [((op x y), p * q) | (x, p) <- toList $ runD d1,
                                                 (y, q) <- toList $ runD d2]

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



evalEE :: Expr -> (Gamma ~> (Either Bool Rational))
evalEE (Var id) = \s -> case E.lookup s id of 
                          Just (R r) -> (return (Right r))
                          Just (B b) -> (return (Left b))
evalEE (RationalConst r) = \s -> return (Right r)
evalEE (Neg r) = \s -> let r' = (evalEE r) s in 
                           (fmap (\p -> case p of 
                                          (Right p') -> Right (-1 * p'))) r'
evalEE (ABinary op e1 e2) = \s -> 
  let e1' = (evalEE e1) s
      e2' = (evalEE e2) s in 
      case op of 
        Add      -> (aOperator (+) e1' e2')
        Subtract -> (aOperator (-) e1' e2')
        Multiply -> (aOperator (*) e1' e2')
        Divide   -> (aOperator (/) e1' e2')
{-
evalEE (Ichoice e1 e2 p) = \s -> 
  let e1' = (evalEE e1) s
      e2' = (evalEE e2) s 
      p'  = (evalEE p) s in 
      case (e1', e2') of 
        (D (Right e1''), e2') -> undefined
    - -}
evalEE (BoolConst b) = \s -> return (Left b)
evalEE (Not b) = \s -> let r' = (evalEE b) s in 
                           (fmap (\bv -> case bv of 
                                          (Left b') -> Left (not b'))) r'
evalEE (BBinary op e1 e2) = \s -> 
  let e1' = (evalEE e1) s
      e2' = (evalEE e2) s in 
      case op of 
        And ->      (bOperator (&&) e1' e2')
        Or  ->      (bOperator (||) e1' e2')
evalEE (RBinary op e1 e2) = \s -> 
  let e1' = (evalEE e1) s
      e2' = (evalEE e2) s in 
      case op of 
        Gt ->      (cOperator (>) e1' e2')
        Ge ->      (cOperator (>=) e1' e2')
        Lt ->      (cOperator (<) e1' e2')
        Le ->      (cOperator (<=) e1' e2')
        Eq ->      (cOperator (==) e1' e2')
        Ne ->      (cOperator (/=) e1' e2')

                                 {-
evalE :: Expr -> Either (Gamma ~> Bool) (Gamma ~> Rational)
evalE (Var id) = \s -> case E.lookup s id of
                         (R r) -> (Right $ return r)
-- evalE (Var id) = \s -> E.lookup s id
-- evalE = undefined
--
-- evalE (Var id) = \s -> case E.lookup s id of 
                         -- (R r) -> (Right (\s -> r))
                         -- (B b) -> (Left b)
evalE (RationalConst r) = (Right (\s -> return r))
evalE (Neg r) = let (Right f) = (evalE r) in 
                    Right (\s -> (fmap ((*) (-1))) (f s)) --- TODO fix the "s"
evalE (ABinary op e1 e2) = 
  let (Right e1') = evalE e1 
      (Right e2') = evalE e2 in 
      -- let e1'' = join (e1' singlePoint)
          -- e2'' = join (e2' singlePoint) in 
          case op of 
            Add      -> Right (\s -> (ariOperate (+) (e1' s) (e2' s)))
            Subtract -> Right (\s -> (ariOperate (-) (e1' s) (e2' s)))
            -- Subtract -> Right (\s -> (ariOperate (-) e1'' e2''))
        {-
evalE (Ichoice e1 e2 p) = 
        let e1' = evalE e1
            e2' = evalE e2
            (Right p') = evalE p in 
            case (e1', e2') of 
              (Left e1'', Left e2'') -> 
                  -}
evalE (BoolConst b) = (Left (\s -> return b))
evalE (Not b) = undefined
evalE (BBinary op e1 e2) = undefined
evalE (RBinary op e1 e2) = 
        let (Right e1') = evalE e1
            (Right e2') = evalE e2 in
            -- let e1'' = join (e1' singlePoint)
                -- e2'' = join (e2' singlePoint) in 
                case op of 
                  Gt -> (Left (\s -> (rCompare (>) (e1' s) (e2' s) )))
                  -}

            

        --(Right (\s -> let (Right f) = (evalE r)  in f (fmap (negate) s)))
-- evalE = undefined

-- example = let (Right p) = (evalE (Neg (RationalConst (1 % 1)))) in 
              -- p (uniform  [10 % 1])
              --
exa :: Expr
-- exa = (ABinary Add (RationalConst (5%1)) (RationalConst (11%1)))
exa = (RationalConst (5%1))

example1 = let (p) = evalEE exa in p E.empty

{-
example1 = let (Right p) = (evalE ((RationalConst (1 % 1)))) in 
              p (uniform  [10 % 1])
              -}

-- example2 = fmap (fmap ((+) 1)) (return (return 1))


        {-
translateP :: Stmt -> (Gamma ~~> Gamma)
translateP (Seq ls) = if length ls > 1 
                      then translateP (head ls) ==> translateP (Seq (tail ls))
                      else translateP (head ls) ==> return
translateP (Assign id expr) = \ds ->
        return (
          case evalE expr of
            (Right sToDr) -> ds >>= (\s -> fmap (\r -> E.add s (id, R r)) (sToDr s))
               )
               -}

        {-
translateKuifje :: Stmt -> Kuifje Gamma 
translateKuifje (Seq []) = skip
translateKuifje (Seq ls) = translateKuifje (head ls) <> translateKuifje (Seq (tail ls))
translateKuifje (Assign id expr) = 
        case evalE expr of 
          (Right sToDr) -> 
                  Language.Kuifje.Syntax.update (\s -> 
                          fmap (\r -> E.add s (id, R r)) (sToDr s))
          (Left sToDb) -> 
                  Language.Kuifje.Syntax.update (\s -> 
                          fmap (\r -> E.add s (id, B r)) (sToDb s))
translateKuifje (Syntax.While e s) = 
        case evalE e of
          (Left sToDb) -> Language.Kuifje.Syntax.while sToDb (translateKuifje s)
          -}

translateKuifje :: Stmt -> Kuifje Gamma 
translateKuifje (Seq []) = skip
translateKuifje (Seq ls) = translateKuifje (head ls) <> translateKuifje (Seq (tail ls))
translateKuifje (Assign id expr) = Language.Kuifje.Syntax.update (\s -> 
        let currS = (evalEE expr) s in
            fmap (\r -> 
                    case r of
                      (Right r) -> E.add s (id, R r)
                      (Left b)  -> E.add s (id, B b)) currS)
translateKuifje (Syntax.While e s) = 
        Language.Kuifje.Syntax.while (\s -> 
                let currS = (evalEE e) s in 
                    fmap (\r -> case r of (Left b) -> b) currS) (translateKuifje s)
translateKuifje (Syntax.If e s1 s2) = 
    Language.Kuifje.Syntax.cond 
        (\s -> let currS = (evalEE e) s in fmap (\r -> case r of (Left b) -> b) currS) 
        (translateKuifje s1) 
        (translateKuifje s2)
translateKuifje Syntax.Skip = skip
translateKuifje (Leak e) = undefined
translateKuifje (Vis s) = undefined
translateKuifje (Echoice s1 s2 p) = undefined



getRational :: Gamma -> String -> Rational
getRational g s | Just (R t) <- E.lookup g s = t
                | otherwise = error "Not going to happen"

project :: Dist (Dist Gamma) -> Dist (Dist Rational)
project = fmap (fmap (\s -> getRational s "y"))

initGamma :: Rational -> Gamma
initGamma x = let g = E.add E.empty ("x", (R x)) in 
               E.add g ("y", (R (0 % 1)))

hyper :: Dist (Dist Rational)
hyper = let g = translateKuifje (Parse.parseString example) in 
            project $ hysem g (uniform [initGamma x | x <- [5..8]])

example :: String
example = "y := 0; while (x > 0) do y := x + y; x := x - 1; od;"

-- example = "y := x + 1;"
          
main :: IO ()
main = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper

        {-
hyper :: Dist (Dist Rational)
hyper = let (g, ve) = program E.empty (Parse.parseString example) in 
            project $ hysem g (uniform [initGamma x | x <- [5..8]])

example :: String
example = "x := 1; y := 0; z := z + 1; while (x > 0) do y := x + y; x := x - 1; od;"


main :: IO ()
main = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper
-}
