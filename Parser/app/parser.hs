
module Parser (kuifjeParser) where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS

import Main (Kuifje, Statement)
-- we parsen kuifje programma in haskell.
-- de parser programma laten runnen maar zo met args 
-- bv. parse somefile.kuifje --seed=123
-- seed is dan gebruikt voor random number generation.


-- -----------------------------------------
-- IMPLEMENTATIE VAN EEN PARSER:
-- -----------------------------------------

kuifjeParser :: PS.Parser Kuifje
kuifjeParser = skipParser P.<|> updateParser P.<|> ifParser P.<|> whileParser

-- TODO:
statementParser :: PS.Parser Statement
statementParser = do
  return (Stat "X" (Lit 5 (Uniform 3 3)))

-- TODO:
condParser :: PS.Parser Cond
condParser = do
  return (Comp (Equal(Lit 5 (Uniform 2 2)) (Lit 5 (Uniform 2 2)) (Bernoulli 2)))

skipParser :: PS.Parser Kuifje
skipParser = do
    P.string "Return"
    return skip

leftBracket :: PS.Parser ()
leftBracket = do
  P.spaces
  P.char '('
  P.spaces

rightBracket :: PS.Parser ()
rightBracket = do
  P.spaces
  P.char ')'
  P.spaces

updateParser :: PS.Parser Kuifje
updateParser = do
  P.string "Update"
  leftBracket
  s <- statementParser
  rightBracket
  k <- kuifjeParser
  return (update s <> k)

ifParser :: PS.Parser Kuifje
ifParser = do
  P.string "If"
  P.spaces
  c <- condParser
  leftBracket
  p <- kuifjeParser
  rightBracket
  leftBracket
  q <- kuifjeParser
  rightBracket
  leftBracket
  r <- kuifjeParser
  rightBracket
  return (cond c p q r)

whileParser :: PS.Parser Kuifje
whileParser = do
  P.string "While"
  P.spaces
  c <- condParser
  leftBracket
  p <- kuifjeParser
  rightBracket
  leftBracket
  q <- kuifjeParser
  rightBracket
  return (While c p q)