module Expr (Expr (..), LispError (..), SymbolTable, IOThrowsError, ThrowsError, liftIOThrow, runIOThrows) where

import Control.Monad.Except
import Data.Functor
import Data.IORef
import qualified Data.Map.Lazy as Map

import Text.Parsec.Error (ParseError)

data Expr = IntegerExpr Integer
          | BooleanExpr Bool
          | StringExpr String
          | SymbolExpr String
          | ListExpr [Expr]
          | LambdaExpr {lmArgs :: [String], lmVarargs :: Maybe String,
                        lmBody :: [Expr], lmClosure :: SymbolTable }
          | DottedListExpr [Expr] Expr
          | PrimitiveFn ([Expr] -> IOThrowsError Expr)

instance Show Expr where
  show (IntegerExpr i) = show i
  show (StringExpr s) = "\"" ++ s ++ "\""
  show (SymbolExpr s) = s
  show (BooleanExpr b) = if b then "#t" else "#f"
  show (ListExpr exprs) = "(" ++ unwords (fmap show exprs) ++ ")"
  show LambdaExpr{lmArgs=args, lmVarargs=varargs, lmBody=_, lmClosure=_} =
    "(lambda (" ++ unwords args ++
      (case varargs of
         Nothing -> ""
         Just v -> " . " ++ show v) ++
      ") ...)"
  show (PrimitiveFn _) = "<primitive>"
  show (DottedListExpr front last) = "(" ++ unwords (fmap show front) ++ " . " ++ show last ++ ")"

type SymbolTable = IORef (Map.Map String (IORef Expr))

data LispError = TypeError String Expr
               | ParserError ParseError
               | FunctionArity Integer [Expr]
               | NotCallable Expr
               | BadSpecialForm String
               | UnboundVariable String
               | DivideByZero
               | EmptyProgram

instance Show LispError where
  show (TypeError t e) = "expected type " ++ t ++ ", got " ++ show e
  show (ParserError pe) = show pe
  show (FunctionArity n exprs) = "function expected " ++ show n ++ " arguments, but got: " ++ unwords (fmap show exprs)
  show (NotCallable e) = show e ++ " is not callable"
  show (BadSpecialForm e) = "bad special form: " ++ e
  show (UnboundVariable v) = "unbound variable: " ++ v
  show DivideByZero = "divide by zero"
  show EmptyProgram = "empty program or function body"

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

liftIOThrow :: ThrowsError a -> IOThrowsError a
liftIOThrow (Left e) = throwError e
liftIOThrow (Right v) = return v

trapError = flip catchError (return . show)
extractValue (Right val) = val

-- TODO: Partial function, gross!
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue
