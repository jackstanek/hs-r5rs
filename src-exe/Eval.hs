module Eval (SymbolTable) where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import qualified Data.Map.Lazy as Map

import Error
import Parse

type SymbolTable = IORef (Map.Map String (IORef Expr))

evaluate :: Expr -> IOThrowsError Expr
evaluate = undefined

