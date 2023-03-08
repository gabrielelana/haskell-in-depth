{-# LANGUAGE StrictData #-}
module Lib (parseExp, evalExp, Exp (..)) where

import Prelude hiding (lex, exp)
import Text.ParserCombinators.Parsec

data Exp a = Lit !a
           | Add !(Exp a) !(Exp a)
           | Mul !(Exp a) !(Exp a)
           deriving (Show, Eq)

lex :: Parser a -> Parser a
lex p = p <* spaces

-- Fail gracefully
num :: Parser (Exp Integer)
num = Lit . read <$> lex (many1 digit)

opa :: Parser Char
opa = lex $ char '+'

opm :: Parser Char
opm = lex $ char '*'

-- <exp> ::= <term> { ("+" | "-") <term> }
-- <term> ::= <factor> { ("*" | "/") <factor> }
-- <factor> ::= "(" <exp> ")" | <int>

exp :: Parser (Exp Integer)
exp = foldl1 Add <$> term `sepBy1` opa

term :: Parser (Exp Integer)
term = foldl1 Mul <$> factor `sepBy1` opm

group :: Parser (Exp Integer)
group = lex (char '(') *> exp <* lex (char ')')

factor :: Parser (Exp Integer)
factor = try group <|> num

-- red :: (Exp a -> Exp a -> Exp a) -> [Exp a] -> Exp a
-- red _ [x] = x
-- red c [x, y] = c x y
-- red c (x:xs) = c x (red c xs)
-- red _ _ = error "impossible"

evalExp :: Exp Integer -> Integer
evalExp (Lit n) = n
evalExp (Add n m) = evalExp n + evalExp m
evalExp (Mul n m) = evalExp n * evalExp m

parseExp :: String -> Either ParseError (Exp Integer)
parseExp = parse (exp <* eof) "expression"
