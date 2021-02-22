module AST (AST (..)) where

import Parse

-- TODO: Implement more derived types
data AST = IntegerAST Integer
         | StringAST String
         | BooleanAST Bool
         | VariableRef String
         | FunctionCall AST [AST]
         | Lambda [String] AST
         | Let [(String, AST)] AST
  deriving Show
