module Main where

import System.Environment
import Kuifje.Run

main = do args <- getArgs
          case args of
            [file]    -> do runHyper file
            otherwise -> error "Please provide a file."
