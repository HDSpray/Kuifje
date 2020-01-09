module Test where

import Parse
import Syntax
import System.Directory
import System.IO 

run :: String -> IO Stmt
run s = do program <- readFile file
           case parse whileParser "" program of
                Left e  -> print e >> fail "parse error"
                Right r -> r
runAll :: [FilePath] -> [Stmt]
runAll [] = []
runAll ls = (run (head ls) ):(run (tail ls))

main = do all <- getDirectoryContents "../tests/"
          return $ runAll all
