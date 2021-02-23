module Error (LispError (..), ThrowsError, IOThrowsError, liftIOThrow, runIOThrows) where

import Control.Monad.Except

import Text.Parsec (ParseError)

import Expr

data LispError = TypeError String Expr
               | ParserError ParseError
               | FunctionArity Integer [Expr]
               | NotCallable Expr
               | BadSpecialForm Expr
               | UnboundVariable String

instance Show LispError where
  show (TypeError t e) = "expected type " ++ t ++ ", got " ++ show e
  show (ParserError pe) = show pe
  show (FunctionArity n exprs) = "function expected " ++ show n ++ " arguments, but got: " ++ unwords (fmap show exprs)
  show (NotCallable e) = show e ++ " is not callable"
  show (BadSpecialForm e) = "bad special form: " ++ show e
  show (UnboundVariable v) = "unbound variable: " ++ v

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

liftIOThrow :: ThrowsError a -> IOThrowsError a
liftIOThrow (Left e) = throwError e
liftIOThrow (Right v) = return v

trapError = flip catchError (return . show)
extractValue (Right val) = val

-- TODO: Partial function, gross!
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
