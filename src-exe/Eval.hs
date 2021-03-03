module Eval (SymbolTable, evaluate, evaluateSeq, primEnv, preludeEnv) where

import Control.Monad
import Control.Monad.Except
import Data.Functor
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as Map

import Expr
import Parse

emptyEnv :: IO SymbolTable
emptyEnv = newIORef Map.empty

-- An environment containing only primitives.
primEnv :: IO SymbolTable
primEnv = emptyEnv >>= bindVars (makePrimitiveFn <$> primitives)
  where primitives = [("+", ringOp ((+), 0)),
                      ("*", ringOp ((*), 1)),
                      ("-", diffQuot (-)),
                      ("/", diffQuot div),
                      ("=", primCmp (==)),
                      ("<", primCmp (<)),
                      (">", primCmp (>)),
                      ("<=", primCmp (<=)),
                      (">=", primCmp (>=)),
                      ("symbol->string", symToString),
                      ("eqv?", eqv),
                      ("car", car),
                      ("cdr", cdr)]
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

        symToString :: [Expr] -> IOThrowsError Expr
        symToString [SymbolExpr sym] = return $ StringExpr sym
        symToString [nonSym] = throwError $ TypeError "symbol" nonSym
        symToString args = throwError $ FunctionArity 1 args

        eqv :: [Expr] -> IOThrowsError Expr
        eqv args = if length args /= 2
                     then throwError $ FunctionArity 2 args
                     else return $ BooleanExpr result
                     where result = case args of
                                      [BooleanExpr l, BooleanExpr r] -> l == r
                                      [StringExpr  l, StringExpr  r] -> l == r
                                      [SymbolExpr  l, SymbolExpr  r] -> l == r
                                      [IntegerExpr l, IntegerExpr r] -> l == r
                                      [ListExpr   [], ListExpr   []] -> True
                                      _ -> False

        car :: [Expr] -> IOThrowsError Expr
        car [ListExpr (head:_)] = return head
        car [ListExpr []] = throwError $ BadSpecialForm "cannot take car of empty list"
        car [DottedListExpr (head:_) _] = return head
        car [nonList] = throwError $ TypeError "list" nonList

        cdr :: [Expr] -> IOThrowsError Expr
        cdr [ListExpr (_:tail)] = return $ ListExpr tail
        cdr [ListExpr []] = throwError $ BadSpecialForm "cannot take cdr of empty list"
        cdr [DottedListExpr (_:tail) final] = return . ListExpr $ tail ++ [final]
        cdr [nonList] = throwError $ TypeError "list" nonList

loadSourceFile :: FilePath -> SymbolTable -> IOThrowsError Expr
loadSourceFile path env = liftIO (readFile path) >>= liftIOThrow . parseProgram path >>= evaluateSeq env

loadSourceDefs :: FilePath -> SymbolTable -> IOThrowsError SymbolTable
loadSourceDefs path env = loadSourceFile path env >> return env

preludeEnv :: IO SymbolTable
preludeEnv = runExceptT (lift primEnv >>= loadSourceDefs "stdlib/std.scm") >>= either stdlibErr return
  where stdlibErr :: LispError -> IO SymbolTable
        stdlibErr err = print err >> primEnv

cloneEnv :: SymbolTable -> IO SymbolTable
cloneEnv env = readIORef env >>= newIORef

evaluate :: SymbolTable -> Expr -> IOThrowsError Expr
evaluate _ val@(IntegerExpr _) = return val
evaluate _ val@(StringExpr _) = return val
evaluate _ val@(BooleanExpr _) = return val

evaluate env (ListExpr [SymbolExpr "load", StringExpr path]) = loadSourceFile path env

evaluate _ (ListExpr [SymbolExpr "quote", val]) = return val
evaluate env (ListExpr [SymbolExpr "if", pred, t, f]) =
    do pred' <- evaluate env pred
       evaluate env (if truthy pred' then t else f)
evaluate env (ListExpr (SymbolExpr "if":_)) = throwError $ BadSpecialForm "malformed if expression"
evaluate env (ListExpr (SymbolExpr "lambda" : ListExpr argSyms : body)) =
    do args <- mapM extractSymbol argSyms
       closure <- liftIO $ cloneEnv env
       return LambdaExpr{lmArgs=args, lmBody=body,
                         lmVarargs=Nothing, lmClosure=closure}
evaluate env (ListExpr (SymbolExpr "define" : ListExpr (SymbolExpr fnname : argSyms) : body)) =
    do args <- mapM extractSymbol argSyms
       closure <- liftIO $ cloneEnv env
       let lm = LambdaExpr{lmArgs=args, lmBody=body,
                           lmVarargs=Nothing, lmClosure=closure}
       liftIO $ bindVar env fnname lm
       liftIO $ bindVar closure fnname lm
       return lm
evaluate env (ListExpr [SymbolExpr "define", SymbolExpr lval, rval]) =
    do r <- evaluate env rval
       liftIO $ bindVar env lval r
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
evaluate _ _ = throwError $ BadSpecialForm "malformed expression"

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


bindVar :: SymbolTable -> String -> Expr -> IO SymbolTable
bindVar env name val = do varRef <- newIORef val
                          modifyIORef' env (Map.insert name varRef)
                          return env

bindVars :: [(String, Expr)] -> SymbolTable -> IO SymbolTable
bindVars bindings env = mapM_ (uncurry $ bindVar env) bindings >> return env

lookupVar :: SymbolTable -> String -> IOThrowsError Expr
lookupVar envRef var = do env <- liftIO $ readIORef envRef
                          case Map.lookup var env of
                            Nothing -> liftIOThrow $ throwError $ UnboundVariable var
                            Just val -> liftIO . readIORef $ val

applyFn :: Expr -> [Expr] -> IOThrowsError Expr
applyFn (PrimitiveFn fn) args = fn args
applyFn (LambdaExpr argNames varargs body closure) args
  | numArgs argNames /= numArgs args = throwError $ FunctionArity (numArgs argNames) args
  | otherwise = do scratch <- liftIO $ cloneEnv closure
                   liftIO $ bindVars (zip argNames args) scratch
                   evaluateSeq scratch body

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
