module Main where

import Control.Monad.Except
import System.Environment

import Parse
import Error
import Eval

main :: IO ()
main = do args <- getArgs
          case args of
            [prog] -> runIOThrows (fmap show $ liftIOThrow (parseExpr "arg" prog) >>= evaluate) >>= putStrLn
            _      -> putStrLn "need exactly one argument."
