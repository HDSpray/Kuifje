{-# LANGUAGE TemplateHaskell #-}

module ReadMeExample where

import Prelude hiding ((!!), return, fmap)
import Control.Lens hiding (Profunctor)
import Data.Semigroup
import qualified Env as E

import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint ()
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax

import Data.Ratio

-- | Function synonym for nicer syntax.
(.^) :: s -> ASetter s t a b -> b -> t
(.^) s x y = set x y s

-- | State space for the program.



data Value = R Rational | B Bool deriving (Show, Eq, Ord)
type Gamma = E.Env Value

-- | Initialize the state by giving a value to x and setting y to 0.
initGamma :: Rational -> Gamma
initGamma x = let g = E.add E.empty ("x", (R x)) in 
               E.add g ("y", (R (0 % 0)))

program :: Kuifje Gamma
program = update (\s -> (toDist s "y" 0)) <>
          while (\s -> (toDistBool s "x" 0)) (
            update (\s -> (toDist s "y" ((getRational s "y") + (getRational s "x")  ))  ) <>
            update (\s -> (toDist s "x" ((getRational s "x") - 1  ))  )
            )

getRational :: Gamma -> String -> Rational
getRational g s | Just (R t) <- E.lookup g s = t
        | otherwise = error "Not going to happen"

toDistBool :: Gamma -> String -> Rational -> Dist Bool
toDistBool g s x | Just (R t) <- E.lookup g s = uniform [(t > x)]
                 | otherwise = error "No"

toDist :: Gamma -> String -> Rational -> Dist Gamma
toDist g s x = uniform [(E.add g (s, (R x)))]

{-
program :: Kuifje SE
program
  = update (\s -> return (s.^y $ 0)) <>
    while (\s -> return (s^.x > 0)) (
      update (\s -> return (s.^y $ (s^.x + s^.y))) <>
      update (\s -> return (s.^x $ (s^.x - 1)))
    )

-}

-- | Extract the meaningful variable from the state space.
project :: Dist (Dist Gamma) -> Dist (Dist Rational)
project = fmap (fmap (\s -> getRational s "y"))

-- | Generate the hyper-distribution for an input of x : [5..8]
-- with uniform distribution.
hyper :: Dist (Dist Rational)
hyper = project $ hysem program (uniform [initGamma x | x <- [5..8]])

main :: IO ()
main = do
  putStrLn "> hyper"
  print hyper
  putStrLn "> condEntropy bayesVuln hyper"
  print $ condEntropy bayesVuln hyper
