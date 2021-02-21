module Main where

import System.Environment

import Parse (parseProgram)
import Eval

main :: IO ()
main = do args <- getArgs
          if length args /= 1
            then putStrLn "need exactly one argument."
            else print (parseProgram "stdin" $ head args)
