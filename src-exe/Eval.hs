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
evaluate env (ListExpr [SymbolExpr "define", SymbolExpr lval, rval]) =
    do r <- evaluate env rval
       liftIO $ bindVar env lval rval
       return r -- note: this behavior is unspecified and
                -- implementation-specific.
evaluate env (ListExpr [SymbolExpr "set!", SymbolExpr lval, rval]) =
    do r <- evaluate env rval
       setVar env lval rval
       return r

evaluate env (ListExpr (SymbolExpr fn : args)) = mapM (evaluate env) args >>= applyFn fn
evaluate env (SymbolExpr var) = lookupVar env var
evaluate _ bad = throwError $ BadSpecialForm bad

setVar :: SymbolTable -> String -> Expr -> IOThrowsError Expr
setVar env name val = do env' <- liftIO $ readIORef env
                         case Map.lookup name env' of
                           Just existing -> do liftIO $ writeIORef existing val
                                               return val
                           Nothing -> throwError $ UnboundVariable name


bindVar :: SymbolTable -> String -> Expr -> IO ()
bindVar env name val = do varRef <- newIORef val
                          modifyIORef' env (Map.insert name varRef)

bindVars :: [(String, Expr)] -> SymbolTable -> IO ()
bindVars bindings env = mapM_ (uncurry $ bindVar env) bindings

lookupVar :: SymbolTable -> String -> IOThrowsError Expr
lookupVar envRef var = do env <- liftIO $ readIORef envRef
                          case Map.lookup var env of
                            Nothing -> liftIOThrow $ throwError $ UnboundVariable var
                            Just val -> liftIO . readIORef $ val

applyFn :: String -> [Expr] -> IOThrowsError Expr
applyFn name args = maybe (throwError $ UnboundVariable name)
                          ($ args)
                          (Map.lookup name primitives)
  where primitives = Map.fromList [("+", ringOp ((+), 0)),
                                   ("*", ringOp ((*), 1)),
                                   ("-", diffQuot (-)),
                                   ("/", diffQuot div),
                                   ("if", ifElse)]
        ringOp :: (Integer -> Integer -> Integer, Integer) -> [Expr] -> IOThrowsError Expr
        ringOp (op, identity) args = mapM extractInteger args <&> (IntegerExpr . foldl' op identity)

        -- TODO: These implementations are not ~quite~ correct. See R5RS section 6.2.5
        diffQuot :: (Integer -> Integer -> Integer) -> [Expr] -> IOThrowsError Expr
        diffQuot op [] = throwError $ FunctionArity 1 []
        diffQuot op (first:rest) = do first' <- extractInteger first
                                      rest' <- mapM extractInteger rest
                                      return $ IntegerExpr $ foldl' op first' rest'

        -- TODO: Add parser support for "if" symtax. There is a slight semantic
        -- difference in argument evaluation order. See R5RS section 4.1.5
        ifElse :: [Expr] -> IOThrowsError Expr
        ifElse [pred, t, f] = return $ if truthy pred
                                         then t
                                         else f
        ifElse args         = throwError $ FunctionArity 3 args

truthy :: Expr -> Bool
truthy (BooleanExpr False) = False
truthy _                   = True

extractInteger :: Expr -> IOThrowsError Integer
extractInteger (IntegerExpr i) = return i
extractInteger other = throwError $ TypeError "integer" other
