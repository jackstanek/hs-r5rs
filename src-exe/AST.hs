module AST (sexpToAst) where

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

getArgNames :: [Sexp] -> Either String [String]
getArgNames = sequenceA . map getArgName
  where getArgName (SymbolExpr s) = Right s
        getArgName _ = Left "invalid type in argument declaration"

getBindings :: [Sexp] -> Either String [(String, AST)]
getBindings = sequenceA . map getBinding
  where getBinding (SExpr ((SymbolExpr name):value:[])) = sexpToAst value >>= \ast -> return (name, ast)
        getBinding _ = Left "malformed binding in let construction" -- TODO: More detail here!

sexpToAst :: Sexp -> Either String AST
sexpToAst sexp = case sexp of
  IntegerExpr i -> return $ IntegerAST i
  BooleanExpr b -> return $ BooleanAST b
  StringExpr s -> return $ StringAST s
  SymbolExpr s -> return $ VariableRef s
  SExpr (SymbolExpr "lambda":SExpr args:body:[]) -> do
    argNames <- getArgNames args
    bodyAst <- sexpToAst body
    return $ Lambda argNames bodyAst
  SExpr (SymbolExpr "lambda":_) -> Left "malformed lambda construction"
  SExpr (SymbolExpr "let":SExpr bindings:body:[]) -> do
    bindingAsts <- getBindings bindings
    bodyAst <- sexpToAst body
    return $ Let bindingAsts bodyAst
  SExpr (SymbolExpr "let":_) -> Left "malformed let construction"
  SExpr (fn:args) -> do
    fnAst <- sexpToAst fn
    argAsts <- sequenceA $ sexpToAst <$> args
    return $ FunctionCall fnAst argAsts
  _ -> Left "malformed expression"
