module Eval (astToEval, execute) where

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
type Program = (Evaluation, EvalContext)
type EvalError = String

astToEval :: AST -> Either EvalError Evaluation
astToEval ast = case ast of
  SymbolExpr s -> Right $ VariableRef s
  otherwise -> Right $ SelfEvaluating ast

evaluate :: Program -> Either EvalError AST
evaluate (eval, env) = case eval of
  VariableRef vname -> varLookup vname
  SelfEvaluating val -> Right val
  a -> Left $ "not implemented: " ++ show a
  where varLookup vname = case Map.lookup vname env of
                            Just val -> evaluate (val, env)
                            Nothing -> Left $ "unbound variable: " ++ vname

topLevel = Map.fromList [("x", SelfEvaluating $ IntegerExpr 69)]

-- Top level execution
execute :: AST -> Either EvalError AST
execute input = astToEval input >>= \eval -> evaluate (eval, topLevel)
