module Kuifje.Evaluator where

import Kuifje.Syntax

import Prelude hiding ((!!), return, fmap)
import Control.Lens hiding (Profunctor)
import Data.Semigroup

import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint ()
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax

evalE :: VENV -> Exp -> Value

