module Main where

import Control.Applicative
import Data.Either (fromRight)

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data AST = IntegerExpr Integer
                | BooleanExpr Bool
                | CharacterExpr Char
                | StringExpr String
                | SymbolExpr String
                | Empty -- i.e. the empty list
                | SExpr AST AST
  deriving (Show, Eq)

lexeme :: Parser a -> Parser a
lexeme p = P.spaces >> p

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

symbolP :: Parser AST
symbolP = lexeme $ peculiarP <|> do
  initial <- initialP
  subsequent <- P.many $ P.choice [initialP, P.digit, P.oneOf "+-.@"]
  return $ SymbolExpr $ initial:subsequent
    where initialP = P.oneOf "!$%&*/:<=>?^_~" <|> P.letter
          peculiarP = SymbolExpr <$> P.choice [P.string "+", P.string "-", P.string "..."]

sexpP :: Parser AST
sexpP = do
  P.char '('
  exprs <- P.many exprP
  P.char ')'
  return $ consify $ exprs
    where consify [] = Empty
          consify (x:xs) = SExpr x $ consify xs

exprP :: Parser AST
exprP = P.choice $ map P.try [intP, symbolP, boolP, sexpP]

main :: IO ()
main = loop "> "
  where loop prompt = do {
    input <- getLine;
    putStrLn . show $ P.parse exprP "" input;
    loop prompt
  }
