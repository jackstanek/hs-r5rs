module Eval (SymbolTable, evaluate, evaluateSeq, primEnv) where

import Control.Monad
import Control.Monad.Except
import Data.Functor
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as Map

import Expr
import Parse

-- An environment containing only primitives.
primEnv :: IO SymbolTable
primEnv = do env <- newIORef Map.empty
             bindVars (makePrimitiveFn <$> primitives) env
             return env
  where primitives = [("+", ringOp ((+), 0)),
                      ("*", ringOp ((*), 1)),
                      ("-", diffQuot (-)),
                      ("/", diffQuot div),
                      ("=", primCmp (==)),
                      ("<", primCmp (<)),
                      (">", primCmp (>)),
                      ("<=", primCmp (<=)),
                      (">=", primCmp (>=))]
        makePrimitiveFn (name, fn) = (name, PrimitiveFn fn)
        ringOp :: (Integer -> Integer -> Integer, Integer) -> [Expr] -> IOThrowsError Expr
        ringOp (op, identity) args = mapM extractInteger args <&> (IntegerExpr . foldl' op identity)

        -- TODO: These implementations are not ~quite~ correct. See R5RS section 6.2.5.
        -- A proper implementation would require a rational type to be added.
        diffQuot :: (Integer -> Integer -> Integer) -> [Expr] -> IOThrowsError Expr
        diffQuot op [] = throwError $ FunctionArity 1 []
        diffQuot op (first:rest) = do first' <- extractInteger first
                                      rest' <- mapM extractInteger rest
                                      return $ IntegerExpr $ foldl' op first' rest'

        primCmp cmp vals@(first:second:rest) = BooleanExpr <$> (mapM extractInteger vals <&> cmpSeq cmp)
        primCmp _ vals = throwError $ FunctionArity 2 vals

        cmpSeq :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
        cmpSeq cmp vals = case vals of
                            []  -> True
                            [_] -> True
                            (first:second:rest) -> (first `cmp` second) && cmpSeq cmp (second:rest)

evaluate :: SymbolTable -> Expr -> IOThrowsError Expr
evaluate _ val@(IntegerExpr _) = return val
evaluate _ val@(StringExpr _) = return val
evaluate _ val@(BooleanExpr _) = return val
evaluate _ (ListExpr [SymbolExpr "quote", val]) = return val
evaluate env (ListExpr [SymbolExpr "if", pred, t, f]) =
    do pred' <- evaluate env pred
       evaluate env (if truthy pred' then t else f)
evaluate env badIf@(ListExpr (SymbolExpr "if":_)) = throwError $ BadSpecialForm badIf
evaluate env (ListExpr (SymbolExpr "lambda" : ListExpr argSyms : body)) =
    do args <- mapM extractSymbol argSyms
       return LambdaExpr{lmArgs=args, lmBody=body,
                         lmVarargs=Nothing, lmClosure=env}
evaluate env (ListExpr (SymbolExpr "define" : ListExpr (SymbolExpr fnname : argSyms) : body)) =
    do args <- mapM extractSymbol argSyms
       let lm = LambdaExpr{lmArgs=args, lmBody=body,
                           lmVarargs=Nothing, lmClosure=env}
       liftIO $ bindVar (lmClosure lm) fnname lm
       return lm
evaluate env (ListExpr [SymbolExpr "define", SymbolExpr lval, rval]) =
    do r <- evaluate env rval
       liftIO $ bindVar env lval rval
       return r -- note: this behavior is unspecified and
                -- implementation-specific.
evaluate env (ListExpr [SymbolExpr "set!", SymbolExpr lval, rval]) =
    do r <- evaluate env rval
       setVar env lval r
       return r

evaluate env (ListExpr (SymbolExpr "list" : elems)) = 
    mapM (evaluate env) elems <&> ListExpr
evaluate env (ListExpr [SymbolExpr "cons", first, rest]) =
    do first' <- evaluate env first
       rest'  <- evaluate env rest
       return $ case rest' of
         ListExpr elems -> ListExpr (first' : elems)
         other          -> DottedListExpr [first'] other

evaluate env (ListExpr (fn : args)) = do evalFn   <- evaluate env fn
                                         evalArgs <- mapM (evaluate env) args
                                         applyFn evalFn evalArgs
evaluate env (SymbolExpr var) = lookupVar env var
evaluate _ lm@LambdaExpr{} = return lm
evaluate _ prim@(PrimitiveFn _) = return prim
evaluate _ bad = throwError $ BadSpecialForm bad

-- Evaluate a sequence of expressions in the same environment
evaluateSeq :: SymbolTable -> [Expr] -> IOThrowsError Expr
evaluateSeq env exprs = mapM (evaluate env) exprs >>= extractLast
  where extractLast :: [Expr] -> IOThrowsError Expr
        extractLast [] = throwError EmptyProgram
        extractLast [last] = return last
        extractLast (_:rest) = extractLast rest

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

applyFn :: Expr -> [Expr] -> IOThrowsError Expr
applyFn (PrimitiveFn fn) args = fn args
applyFn (LambdaExpr argNames varargs body closure) args
  | numArgs argNames /= numArgs args = throwError $ FunctionArity (numArgs argNames) args
  | otherwise = do liftIO $ bindVars (zip argNames args) closure
                   evaluateSeq closure body

  where numArgs = toInteger . length

applyFn notFn _ = throwError $ NotCallable notFn

truthy :: Expr -> Bool
truthy (BooleanExpr False) = False
truthy _                   = True

extractInteger :: Expr -> IOThrowsError Integer
extractInteger (IntegerExpr i) = return i
extractInteger other = throwError $ TypeError "integer" other

extractSymbol :: Expr -> IOThrowsError String
extractSymbol (SymbolExpr s) = return s
extractSymbol other = throwError $ TypeError "symbol" other

isList (ListExpr _) = True
isList _            = False
