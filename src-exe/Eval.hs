module Eval where

import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.Map.Lazy as Map
import Data.Maybe (maybe)

import AST
import Parse

type Evaluation = StateT (Map.Map String AST) (Either String)

evalAST :: Evaluation AST -> Either String Sexp
evalAST ast = do
  
