module Parse (Sexp (..), parseProgram) where

import Control.Applicative

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data Sexp = IntegerExpr Integer
          | BooleanExpr Bool
          | StringExpr String
          | SymbolExpr String
          | Empty -- i.e. the empty list
          | SExpr Sexp Sexp
  deriving Eq

instance Show Sexp where
  show (IntegerExpr i) = show i
  show (StringExpr s) = "\"" ++ s ++ "\""
  show (SymbolExpr s) = s
  show (BooleanExpr b) = if b then "#t" else "#f"
  show (Empty) = "()"
  show (SExpr first rest) = "(" ++ show first ++ " " ++ showRest rest ++ ")"
    where showRest (SExpr next Empty) = show next
          showRest (SExpr next rest) = show next ++ " " ++ showRest rest
          showRest other = ". " ++ show other

comment = do
  P.string ";"
  P.many $ P.noneOf "\n"

ignored = P.many $ P.choice [comment, P.many1 P.space]

lexeme :: Parser a -> Parser a
lexeme p = ignored *> p <* ignored

-- Parse boolean literals ("#t" and "#f")
boolP :: Parser Sexp
boolP = lexeme $ do
  P.char '#'
  value <- P.oneOf "tf"
  return $ BooleanExpr (value == 't')

intP :: Parser Sexp
intP = lexeme $ do
  sign <- P.optionMaybe $ P.oneOf "+-"
  value <- P.many1 P.digit
  return $ IntegerExpr $ multiplier sign * (read value)
    where multiplier sign = case sign of
                              Just '-' -> -1
                              otherwise -> 1

-- Parse string literals
stringP :: Parser Sexp
stringP = lexeme $ do
  P.char '\"'
  contents <- P.many $ P.noneOf "\""
  P.char '\"'
  return $ StringExpr $ contents

-- Parse symbols (referred to as identifiers in the spec)
symbolP :: Parser Sexp
symbolP = lexeme $ peculiarP <|> do
  initial <- initialP
  subsequent <- P.many $ P.choice [initialP, P.digit, P.oneOf "+-.@"]
  return $ SymbolExpr $ initial:subsequent
    where initialP = P.oneOf "!$%&*/:<=>?^_~" <|> P.letter
          peculiarP = SymbolExpr <$> P.choice [P.string "+", P.string "-", P.string "..."]

-- helper function to create lists
consify :: Sexp -> [Sexp] -> Sexp
consify last [] = last
consify last (x:xs) = SExpr x $ consify last xs
buildList = consify Empty

-- Simple parsers for parentheses
lparen = lexeme $ P.char '('
rparen = lexeme $ P.char ')'

-- Parser for simple list notation
listP :: Parser Sexp
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

exprP :: Parser Sexp
exprP = P.choice $ map P.try [intP, stringP, symbolP, boolP, listP, pairP, quotedP]

quotedP = do
  P.char '\''
  quoted <- exprP
  return $ SExpr (SymbolExpr "quote") (SExpr quoted Empty)

programP = P.sepBy1 exprP ignored

parseProgram = P.parse exprP
