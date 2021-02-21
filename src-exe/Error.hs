module Error (LispError (..), ThrowsError, IOThrowsError, ioThrowError) where

import Control.Monad.Except

import Text.Parsec (ParseError)

import Expr

data LispError = TypeError String Expr
               | ParserError ParseError
               | FunctionArity Integer [Expr]
               | NotCallable Expr
               | BadSpecialForm String Expr
               | UnboundVariable Expr

instance Show LispError where
  show (TypeError t e) = "expected type " ++ t ++ ", got " ++ show e
  show (ParserError pe) = show pe
  show (FunctionArity n exprs) = "function expected " ++ show n ++ " arguments, but got: " ++ unwords (fmap show exprs)
  show (NotCallable e) = show e ++ " is not callable"
  show (BadSpecialForm m e) = m ++ ": " ++ show e
  show (UnboundVariable v) = "unbound variable: " ++ show v

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

ioThrowError :: ThrowsError a -> IOThrowsError a
ioThrowError (Left e) = throwError e
ioThrowError (Right v) = return v
