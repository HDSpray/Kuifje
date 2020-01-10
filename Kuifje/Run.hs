module Run where

import Translator
import System.IO 
import Prelude hiding ((!!), fmap, (>>=))
import qualified Env as E

import Language.Kuifje.Distribution
import PrettyPrint 
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax
import Parse
import Syntax

project :: Dist (Dist Gamma) -> Dist (Dist Rational)
project = fmap (fmap (\s -> getRational s "x"))

runHyper s = do tmp <- parseFile s
                let g = translateKuifje tmp
                Prelude.return $ Run.project $ hysem g (uniform [E.empty])

runFile :: String -> IO ()
runFile s = do
  putStrLn "> hyper"
  p <- runHyper s
  print p
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln p

