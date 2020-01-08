module Example where

import Parse
import Syntax
import Translator

import Prelude hiding (fmap)
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


hyperP :: String -> Dist (Dist Rational)
hyperP example = let g = translateKuifje (Parse.parseString example) in 
            project2 $ hysem g (uniform [initGamma x | x <- [5..8]])

project2 :: Dist (Dist Gamma) -> Dist (Dist Rational)
project2 = fmap (fmap (\s -> getRational s "x"))

run :: String -> IO ()
run file =
        do program  <- readFile file 
           putStrLn "> hyper"
           print (hyperP program)
           putStrLn "> condEntropy bayesVuln hyper" 
           print $ condEntropy bayesVuln (hyperP program)
