module Parse (Expr (..), parseProgram) where

import Control.Applicative

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec.Pos

import Error
import Expr

comment = do
  P.string ";"
  P.many $ P.noneOf "\n"

ignored = P.many $ P.choice [comment, P.many1 P.space]

lexeme :: Parser a -> Parser a
lexeme p = ignored *> p <* ignored

-- Parse boolean literals ("#t" and "#f")
boolP :: Parser Expr
boolP = lexeme $ do
  P.char '#'
  value <- P.oneOf "tf"
  return $ BooleanExpr (value == 't')

intP :: Parser Expr
intP = lexeme $ do
  sign <- P.optionMaybe $ P.oneOf "+-"
  value <- P.many1 P.digit
  return $ IntegerExpr $ multiplier sign * read value
    where multiplier sign = case sign of
                              Just '-' -> -1
                              _        -> 1

-- Parse string literals
stringP :: Parser Expr
stringP = lexeme $ do
  P.char '\"'
  contents <- P.many $ P.noneOf "\""
  P.char '\"'
  return (StringExpr contents)

-- Parse symbols (referred to as identifiers in the spec)
symbolP :: Parser Expr
symbolP = lexeme $ peculiarP <|> do
  initial <- initialP
  subsequent <- P.many $ P.choice [initialP, P.digit, P.oneOf "+-.@"]
  return $ SymbolExpr $ initial:subsequent
    where initialP = P.oneOf "!$%&*/:<=>?^_~" <|> P.letter
          peculiarP = SymbolExpr <$> P.choice [P.string "+", P.string "-", P.string "..."]

-- Simple parsers for parentheses
lparen = lexeme $ P.char '('
rparen = lexeme $ P.char ')'

-- Parser for 

-- Parser for simple list notation
listP :: Parser Expr
listP = do
  lparen
  exprs <- P.many exprP
  rparen
  return (ListExpr exprs)

exprP :: Parser Expr
exprP = P.choice $ map P.try [intP, stringP, symbolP, boolP, listP, quotedP]

quotedP = do
  P.char '\''
  quoted <- exprP
  return $ ListExpr [SymbolExpr "quote", quoted]

programP = P.sepBy1 exprP ignored

parseProgram :: Text.Parsec.Pos.SourceName -> String -> ThrowsError [Expr]
parseProgram source input = case P.parse programP source input of
  Left err -> Left (ParserError err)
  Right result -> Right result

