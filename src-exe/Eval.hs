module Eval (runExpr, evalProgram, SymbolTable) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import qualified Data.Map.Lazy as Map

import Error
import Parse

success = lift . Right
failure = lift . Left

type SymbolTable = (Map.Map String Expr)

evaluate :: Expr -> StateT SymbolTable ThrowsError Expr
evaluate val@(IntegerExpr _) = success val
evaluate val@(BooleanExpr _) = success val
evaluate val@(StringExpr _)  = success val
evaluate (ListExpr [SymbolExpr "quote", val]) = success val
evaluate (ListExpr [SymbolExpr "define", SymbolExpr lval, rval]) = do
  finalVal <- evaluate rval
  modify (Map.insert lval rval)
  return rval
evaluate (ListExpr (fn : args)) = do
  fnObj <- evaluate fn
  argsObjs <- traverse evaluate args
  return fnObj -- TODO: actually apply the function!
evaluate e@(SymbolExpr var) = do
  env <- get
  case Map.lookup var env of
    Just val -> success val
    Nothing -> failure (UnboundVariable e)

runExpr :: Expr -> ThrowsError (Expr, SymbolTable)
runExpr program = runStateT (evaluate program) Map.empty

evalProgram :: [Expr] -> ThrowsError Expr
evalProgram exprs = evalStateT (last <$> results) Map.empty
  where results = mapM evaluate exprs
