module Expr (Expr (..)) where

data Expr = IntegerExpr Integer
          | BooleanExpr Bool
          | StringExpr String
          | SymbolExpr String
          | ListExpr [Expr]
          | LambdaExpr ([Expr] -> Expr)

instance Show Expr where
  show (IntegerExpr i) = show i
  show (StringExpr s) = "\"" ++ s ++ "\""
  show (SymbolExpr s) = s
  show (BooleanExpr b) = if b then "#t" else "#f"
  show (ListExpr exprs) = "(" ++ unwords (fmap show exprs) ++ ")"
  show (LambdaExpr _) = "<function>"

