module Parse (AST, parseExpr) where

import Control.Applicative

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data AST = IntegerExpr Integer
                | BooleanExpr Bool
                | StringExpr String
                | SymbolExpr String
                | Empty -- i.e. the empty list
                | SExpr AST AST
  deriving (Eq, Show)

lexeme :: Parser a -> Parser a
lexeme p = P.spaces *> p

-- Parse boolean literals ("#t" and "#f")
boolP :: Parser AST
boolP = lexeme $ do
  P.char '#'
  value <- P.oneOf "tf"
  return $ BooleanExpr (value == 't')

intP :: Parser AST
intP = lexeme $ do
  sign <- P.optionMaybe $ P.oneOf "+-"
  value <- P.many1 P.digit
  return $ IntegerExpr $ multiplier sign * (read value)
    where multiplier sign = case sign of
                              Just '-' -> -1
                              otherwise -> 1

-- Parse string literals
stringP :: Parser AST
stringP = lexeme $ do
  P.char '\"'
  contents <- P.many $ P.noneOf "\""
  P.char '\"'
  return $ StringExpr $ contents

-- Parse symbols (referred to as identifiers in the spec)
symbolP :: Parser AST
symbolP = lexeme $ peculiarP <|> do
  initial <- initialP
  subsequent <- P.many $ P.choice [initialP, P.digit, P.oneOf "+-.@"]
  return $ SymbolExpr $ initial:subsequent
    where initialP = P.oneOf "!$%&*/:<=>?^_~" <|> P.letter
          peculiarP = SymbolExpr <$> P.choice [P.string "+", P.string "-", P.string "..."]

-- helper function to create lists
consify :: AST -> [AST] -> AST
consify last [] = last
consify last (x:xs) = SExpr x $ consify last xs
buildList = consify Empty

-- Simple parsers for parentheses
lparen = lexeme $ P.char '('
rparen = lexeme $ P.char ')'

-- Parser for simple list notation
listP :: Parser AST
listP = do
  lparen
  exprs <- P.many exprP
  rparen
  return $ buildList $ exprs

-- Parser for pairs (e.g. "(1 . 2)")
pairP = do
  lparen
  frontExprs <- P.many1 exprP
  lexeme $ P.char '.'
  lastExpr <- exprP
  rparen
  return $ consify lastExpr frontExprs

exprP :: Parser AST
exprP = P.choice $ map P.try [intP, stringP, symbolP, boolP, listP, pairP, quotedP]

quotedP = do
  P.char '\''
  quoted <- exprP
  return $ SExpr (SymbolExpr "quote") quoted

programP = P.many1 $ lexeme $ exprP

parseExpr = P.parse exprP
