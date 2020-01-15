module Run where

import Translator
import System.IO 
import Prelude hiding ((!!), fmap, (>>=))
import qualified Env as E

import Language.Kuifje.Distribution
import PrettyPrint 
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax
import Data.Map.Strict
import Data.List
-- import qualified Data.Set as DSET
import Parse
import Syntax



        {-
getFrom g s | Just (B b) <- E.lookup g s = (Left b)
            | Just (R r) <- E.lookup g s = (Right r)
            | otherwise = error ("Not going to happend " ++ s)
            -}
getFrom g s | Just x <- E.lookup g s = x
            | otherwise = error ("Not going to happend " ++ s)
            

-- project :: String -> Dist (Dist Gamma) -> Dist (Dist (Either Bool Rational))
-- project var = fmap (fmap (\s -> getFrom s var))

project :: String -> Dist (Dist Gamma) -> Dist (Dist Value)
project var = fmap (fmap (\s -> getFrom s var))

runHyper s = do tmp <- parseFile s
                let g = translateKuifje tmp
                let kuifje = hysem g (uniform [E.empty])
                let (env, _) = (toList $ runD kuifje) !! 0
                let (gamma, _) = ((toList $ runD $ env) !! 0)
                let all_var = E.allVar gamma
                outputL [(x, Run.project x kuifje) | x <- all_var]
                -- Prelude.return $ Run.project "y" kuifje


outputL (ls) = if length ls == 1 
                  then do putStrLn $ "> Variable " ++ (fst $ head ls) ++ " hyper"
                          print $ snd $ head ls
                          putStrLn "> condEntropy bayesVuln hyper"
                          putStrLn ""
                          print $ condEntropy bayesVuln $ snd $ head ls
                  else do putStrLn $ "> Variable " ++ (fst $ head ls) ++ " hyper"
                          print $ snd $ head ls
                          putStrLn "> condEntropy bayesVuln hyper"
                          print $ condEntropy bayesVuln $ snd $ head ls
                          putStrLn ""
                          outputL $ tail ls
runFile :: String -> IO ()
runFile s = do
  runHyper s
  -- print p
  -- putStrLn "> condEntropy bayesVuln hyper"
  -- print $ condEntropy bayesVuln p

