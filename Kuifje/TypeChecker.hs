module Kuifje.TypeChecker where

import qualified Kuifje.Env as E
import Kuifje.Syntax

import Control.Applicative
import Control.Monad (void, unless)

type Gamma = E.Env Type

primOpType :: Op -> Type
