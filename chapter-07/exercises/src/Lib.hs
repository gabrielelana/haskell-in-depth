{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Char

-- 1. Implement a parser for the following grammar of a simple programming
--    language:

data StmtW = SeqW ![StmtW]
           | WhileW !ExpW !StmtW
           | AssignW String !ExpW
           deriving (Show, Eq)

data ExpW = IfW !ExpW !ExpW !ExpW
          | CmpW !CmpSW !AxpW !AxpW
          | NotW !ExpW
          | AxpW !AxpW
          deriving (Show, Eq)

data CmpSW = LTEW | LTW | GTW | GTEW | EQW | NEQW deriving (Show, Eq)

data AxpW = NumW !Integer
          | IdW !String
          | OpW !OpSW !AxpW !AxpW
          deriving (Show, Eq)

data OpSW = ADDW | SUBW | MULW | DIVW deriving (Show, Eq)

-- stmts :: = stmt ’;’ stmts
--          | stmt
-- stmt :: = ’while’ exp ’do’ stmts ’done’
--         | id ’: =’ exp
-- exp :: = ’if’ exp ’then’ exp ’else’ exp ’fi’
--        | aexp cmp aexp
--        | ’not’ exp
--        | aexp
-- aexp :: = num
--         | id
--         | ’(’ aexp op aexp ’)’
-- cmp :: = ’<=’ | ’>’ | ’==’ | ’! =’
-- op :: = ’+’ | ’-’ | ’*’ | ’/’
-- num :: = "[0-9] +"
-- id :: = "[a-zA-Z] [a-zA-Z0-9] *"

-- An example
--
-- x := 0; y := 5;
-- while x <= 10 do
--   y := (y * 5); x := (x + 1)
-- done;
-- y := if y> 10000 then 10000 else y fi

parserW :: Parsec String () StmtW
parserW = stmtW <* spaces <* eof

whileW :: Parsec String () StmtW
whileW = do
  _ <- symW "while"
  c <- lexW expW
  _ <- symW "do"
  r <- lexW stmtW
  _ <- symW "done"
  return $ WhileW c r

assignW :: Parsec String () StmtW
assignW = do
  (IdW s) <- lexW idW
  _ <- symW ":="
  v <- lexW expW
  return $ AssignW s v

seqW :: Parsec String () StmtW
seqW = SeqW <$> sepBy1 stmtW (symW ";")

stmtW :: Parsec String () StmtW
stmtW = whileW <|> try assignW <|> seqW <?> "statements"

ifW :: Parsec String () ExpW
ifW = do
  _ <- symW "if"
  c <- lexW expW
  _ <- symW "then"
  t <- lexW expW
  _ <- symW "else"
  f <- lexW expW
  _ <- symW "fi"
  return $ IfW c t f

notW :: Parsec String () ExpW
notW = NotW <$> (symW "not" *> expW)

cmpW :: Parsec String () ExpW
cmpW = do
  l <- lexW axpW
  o <- lexW opSW
  r <- lexW axpW
  return $ CmpW o l r
    where opSW = LTEW <$ try (symW "<=")
             <|> GTEW <$ try (symW ">=")
             <|> LTW <$ try (symW "<")
             <|> GTW <$ try (symW ">")
             <|> EQW <$ symW "=="
             <|> NEQW <$ symW "!="

expW :: Parsec String () ExpW
expW = try ifW <|> try notW <|> try cmpW <|> (AxpW <$> axpW) <?> "expression"

lexW :: Parsec String u a -> Parsec String u a
lexW p = many (oneOf " \t\n") *> p

symW :: String -> Parsec String u String
symW s = lexW $ string s

numW :: Parsec String () AxpW
numW = NumW . read <$> many1 digit

idW :: Parsec String () AxpW
idW = IdW <$> (many1 letter <> many alphaNum)

opW :: Parsec String () AxpW
opW = do
  _ <- symW "("
  l <- lexW axpW
  o <- lexW opSW
  r <- lexW axpW
  _ <- symW ")"
  return $ OpW o l r
    where opSW = ADDW <$ symW "+"
             <|> SUBW <$ symW "-"
             <|> MULW <$ symW "*"
             <|> DIVW <$ symW "/"

axpW :: Parsec String () AxpW
axpW = try opW <|> try numW <|> idW <?> "aritmeic expression"

-- 2. Define the interpreter monad featuring state and failure. Which monad do
--    you choose and in which order?


-- 3. Introduce boolean variables. The interpreter should fail if a integer
--    variable is used in a boolean operation, and conversely


-- 4. Add printing statements to the language. Which monad transformer do you
--    use for this task?
