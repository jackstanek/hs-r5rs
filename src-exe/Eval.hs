module Eval (SymbolTable, evaluate, empty) where

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
empty :: IO SymbolTable
empty = newIORef Map.empty

evaluate :: SymbolTable -> Expr -> IOThrowsError Expr
evaluate _ val@(IntegerExpr _) = return val
evaluate _ val@(StringExpr _) = return val
evaluate _ val@(BooleanExpr _) = return val
evaluate _ (ListExpr [SymbolExpr "quote", val]) = return val
evaluate env (ListExpr (SymbolExpr fn : args)) = mapM (evaluate env) args >>= applyFn fn
evaluate env (SymbolExpr var) = lookupVar env var
evaluate _ bad = throwError $ BadSpecialForm bad

lookupVar :: SymbolTable -> String -> IOThrowsError Expr
lookupVar envRef var = do env <- liftIO $ readIORef envRef
                          case Map.lookup var env of
                            Nothing -> liftIOThrow $ throwError $ UnboundVariable var
                            Just val -> liftIO . readIORef $ val

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
