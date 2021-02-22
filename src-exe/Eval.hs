module Eval (SymbolTable, evaluate) where

import Control.Monad
import Control.Monad.Except
import Data.Functor
import Data.List (foldl')
import Data.IORef
import qualified Data.Map.Lazy as Map

import Expr
import Error
import Parse

type SymbolTable = IORef (Map.Map String (IORef Expr))

evaluate :: Expr -> IOThrowsError Expr
evaluate val@(IntegerExpr _) = return val
evaluate val@(StringExpr _) = return val
evaluate val@(BooleanExpr _) = return val
evaluate (ListExpr [SymbolExpr "quote", val]) = return val
evaluate (ListExpr (SymbolExpr fn : args)) = mapM evaluate args >>= applyFn fn
evaluate bad = throwError $ BadSpecialForm bad

applyFn :: String -> [Expr] -> IOThrowsError Expr
applyFn name args = maybe (throwError $ UnboundVariable name)
                          ($ args)
                          (Map.lookup name primitives)
  where primitives = Map.fromList [("+", numericOp ((+), 0)),
                                   ("*", numericOp ((*), 1))]
        numericOp :: (Integer -> Integer -> Integer, Integer) -> [Expr] -> IOThrowsError Expr
        numericOp (op, identity) args = mapM extractInteger args <&> (IntegerExpr . foldl' op identity)

extractInteger :: Expr -> IOThrowsError Integer
extractInteger (IntegerExpr i) = return i
extractInteger other = throwError $ TypeError "integer" other
