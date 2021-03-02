module Main where

import Control.Monad.Except

import System.Console.Haskeline

import Eval
import Expr
import Parse

main :: IO ()
main = runInputT defaultSettings (lift primEnv >>= loop)
       where loop :: SymbolTable -> InputT IO ()
             loop env = do minput <- getInputLine ">>> "
                           case minput of
                             Nothing -> return ()
                             Just ":q" -> return ()
                             Just input -> do result <- liftRun (fmap show $ liftIOThrow (parseExpr "arg" input) >>= evaluate env)
                                              outputStrLn result
                                              loop env
             liftRun = lift . runIOThrows
