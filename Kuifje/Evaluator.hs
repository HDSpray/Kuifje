{-# LANGUAGE TemplateHaskell #-}
module Evaluator where

import Parse
import Syntax

import Prelude hiding ((!!), return, fmap)
import Control.Lens hiding (Profunctor)
import Data.Semigroup

import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint ()
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax

import System.IO 
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


data SE = SE {
  _x :: Integer,
  _y :: Integer
  } deriving (Eq, Ord)
makeLenses ''SE


parseString :: String -> Stmt
parseString str =
        case parse whileParser "" str of
          Left e  -> error $ show e
          Right r -> r


program :: Stmt -> Kuifje SE
program (Seq ls) = if length ls > 1 
                    then program (head ls) <> program (Seq (tail ls))
                    else program (head ls)
program (SVar id) = undefined -- todo
program (Assign id expr) = 



