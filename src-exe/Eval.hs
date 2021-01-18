module Eval (astToEval, execute, evaluate, EvalContext, topLevel) where

import qualified Data.Map.Lazy as Map
import Data.Maybe (maybe)

import Parse

data Evaluation = SelfEvaluating AST
                | VariableRef String
                | Lambda [String] [Evaluation]
                | Let [(String, Evaluation)]
                | ProcCall Evaluation [Evaluation]
  deriving Show

type EvalContext = Map.Map String Evaluation
type EvalError = String

astToEval :: AST -> Either EvalError Evaluation
astToEval ast = case ast of
  SymbolExpr s -> Right $ VariableRef s
  otherwise -> Right $ SelfEvaluating ast

evaluateVarLookup :: EvalContext -> String -> Either EvalError (EvalContext, AST)
evaluateVarLookup env vname = case Map.lookup vname env of
  Just val -> evaluate env val
  Nothing -> Left $ "unbound variable: " ++ vname

evaluate :: EvalContext -> Evaluation -> Either EvalError (EvalContext, AST)
evaluate env eval = case eval of
  VariableRef vname -> evaluateVarLookup env vname
  SelfEvaluating val -> Right (env, val)
  a -> Left $ "not implemented: " ++ show a

execute :: EvalContext -> AST -> Either EvalError (EvalContext, AST)
execute env ast = astToEval ast >>= evaluate env

topLevel = Map.empty
