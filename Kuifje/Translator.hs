{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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

evalE :: Expr -> (s ~~> s)
evalE 

translateP :: (Ord s) => Stmt -> (s ~~> s)
translateP (Seq ls) = if length ls > 1 
                      then translateP (head ls) ==> translateP (Seq (tail ls))
                      else translateP (head ls) ==> return
translateP (Assign id expr) = undefined

hyper :: Dist (Dist Rational)
hyper = let (g, ve) = program E.empty (Parse.parseString example) in 
            project $ hysem g (uniform [initGamma x | x <- [5..8]])

example :: String
example = "x := 1; y := 0; z := z + 1; while (x > 0) do y := x + y; x := x - 1; od;"

initGamma :: Rational -> Gamma
initGamma x = let g = E.add E.empty ("x", (R x)) in E.add g ("y", (R (0 % 1)))

main :: IO ()
main = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper
