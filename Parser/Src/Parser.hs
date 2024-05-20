module Src.Parser (parser) where

import qualified Text.Parsec.String as PS
import qualified Src.Representation as Rp
import qualified Src.Distributions as Ds
import qualified Text.Parsec as P
import qualified Data.Char as C

import Debug.Trace (trace)

-- -------------------------------------------------------
-- -------------------------------------------------------
--          UTIL PARSER
-- -------------------------------------------------------
-- -------------------------------------------------------

token :: PS.Parser a -> PS.Parser a
token p = p <* P.spaces

digit :: PS.Parser Rp.Expr
digit = do
  x <- token (P.satisfy C.isDigit)
  return (Rp.Lit (C.ord x - C.ord '0') Ds.litDist)

variable :: PS.Parser Rp.Expr
variable = do
  x <- P.many1 P.letter
  return (Rp.Var x)

parseVar :: PS.Parser String
parseVar = P.many1 P.letter

symb :: String -> PS.Parser String
symb w = token (P.string w)

-- -------------------------------------------------------------
-- -------------------------------------------------------------

addop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
addop = do {symb "+"; return (\x y -> Rp.Add x y Ds.addDist)}

subop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
subop = do {symb "-"; return (\x y -> Rp.Sub x y Ds.subDist)}

addsubop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
addsubop =  addop P.<|> subop

-- -------------------------------------------------------------
-- -------------------------------------------------------------

mulop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
mulop = do {symb "*"; return (\x y -> Rp.Mul x y Ds.mulDist)}

divop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
divop = do {symb "/"; return (\x y -> Rp.Div x y Ds.divDist)}

modop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
modop = do {symb "%"; return (\x y -> Rp.Mod x y Ds.divDist)}

muldivop :: PS.Parser (Rp.Expr -> Rp.Expr -> Rp.Expr)
muldivop = mulop P.<|> divop P.<|> modop

-- -------------------------------------------------------------
-- -------------------------------------------------------------

factor :: PS.Parser Rp.Expr
factor = digit P.<|> variable P.<|> do {symb "("; n <- parseExpr ; symb ")"; return n}

term :: PS.Parser Rp.Expr
term = factor `P.chainl1` muldivop

parseExpr :: PS.Parser Rp.Expr
parseExpr = term `P.chainl1` addsubop

-- -------------------------------------------------------
-- -------------------------------------------------------
--          KUIFJE LANGUAGE PARSER
-- -------------------------------------------------------
-- -------------------------------------------------------

parseComp :: PS.Parser Rp.Comp
parseComp = P.try parseEqual  P.<|> P.try parseNotEqual
  P.<|> P.try parseLessThan P.<|> P.try parseGreaterThan
  P.<|> P.try parseLessThanOrEqual P.<|> P.try parseGreaterThanOrEqual

parseEqual :: PS.Parser Rp.Comp
parseEqual = do
  e1 <- parseExpr
  P.spaces
  P.string "=="
  P.spaces
  e2 <- parseExpr
  -- let message = "Parsed equal 3: " ++ show e2 
  -- trace message $ 
  return (Rp.Equal e1 e2 Ds.boolDist)

parseNotEqual :: PS.Parser Rp.Comp
parseNotEqual = do
  e1 <- parseExpr
  P.spaces
  P.string "!="
  P.spaces
  e2 <- parseExpr
  return (Rp.NotEqual e1 e2 Ds.boolDist)

parseLessThan :: PS.Parser Rp.Comp
parseLessThan = do
  e1 <- parseExpr
  P.spaces
  P.char '<'
  P.spaces
  e2 <- parseExpr
  return (Rp.LessThan e1 e2 Ds.boolDist)

parseGreaterThan :: PS.Parser Rp.Comp
parseGreaterThan = do
  e1 <- parseExpr
  P.spaces
  P.char '>'
  P.spaces
  e2 <- parseExpr
  return (Rp.GreaterThan e1 e2 Ds.boolDist)

parseLessThanOrEqual :: PS.Parser Rp.Comp
parseLessThanOrEqual = do
  e1 <- parseExpr
  P.spaces
  P.string "<="
  P.spaces
  e2 <- parseExpr
  return (Rp.LessThanOrEqual e1 e2 Ds.boolDist)

parseGreaterThanOrEqual :: PS.Parser Rp.Comp
parseGreaterThanOrEqual = do
  e1 <- parseExpr
  P.spaces
  P.string ">="
  P.spaces
  e2 <- parseExpr
  return (Rp.GreaterThanOrEqual e1 e2 Ds.boolDist)

parseCond :: PS.Parser Rp.Cond
parseCond = parseCompCond P.<|> parseAndCond P.<|> parseOrCond

parseCompCond :: PS.Parser Rp.Cond
parseCompCond = do
  Rp.Comp <$> parseComp

parseAndCond :: PS.Parser Rp.Cond
parseAndCond = do
  c1 <- parseCond
  P.spaces
  P.string "&&"
  P.spaces
  c2 <- parseCond
  return (Rp.And c1 c2 Ds.boolDist)

parseOrCond :: PS.Parser Rp.Cond
parseOrCond = do
  c1 <- parseCond
  P.spaces
  P.string "||"
  P.spaces
  c2 <- parseCond
  return (Rp.Or c1 c2 Ds.boolDist)

parseStatement :: PS.Parser Rp.Statement
parseStatement = do
  v <- parseVar
  P.spaces
  P.char '='
  P.spaces
  e <- parseExpr
  P.char ';'
  -- let message = "Parsed Var: " ++ show v  ++ " Parsed Expr: " ++ show e
  -- trace message $ return (Rp.Stat "x" (Rp.Lit 2 Ds.litDist))
  return (Rp.Stat v e)

parseKuifje :: PS.Parser Rp.Kuifje
parseKuifje = parseIf P.<|> parseWhile P.<|> parseReturn P.<|> parseUpdate P.<|> parseSkip

parseSkip :: PS.Parser Rp.Kuifje
parseSkip = do
  return Rp.Skip

parseReturn :: PS.Parser Rp.Kuifje
parseReturn = do
  P.string "return"
  P.spaces
  v <- parseVar
  P.char ';'
  return (Rp.Return v)

parseUpdate :: PS.Parser Rp.Kuifje
parseUpdate = do
  s <- parseStatement
  P.spaces
  Rp.Update s <$> parseKuifje

parseIf :: PS.Parser Rp.Kuifje
parseIf = do
  P.string "if"
  P.spaces
  P.string "((("
  P.spaces
  c <- parseCond
  P.spaces
  P.string ")))"
  P.char '{'
  k1 <- parseKuifje
  P.char '}'
  P.spaces
  P.string "else"
  P.spaces
  P.char '{'
  k2 <- parseKuifje
  P.char '}'
  P.spaces
  Rp.If c k1 k2 <$> parseKuifje

parseWhile :: PS.Parser Rp.Kuifje
parseWhile = do
  P.string "while"
  P.spaces
  P.string "((("
  P.spaces
  c <- parseCond
  P.spaces
  P.string ")))"
  P.char '{'
  k1 <- parseKuifje
  P.char '}'
  P.spaces
  Rp.While c k1 <$> parseKuifje

removeNewlines :: String -> String
removeNewlines = filter (/= '\n')

parser :: IO Rp.Kuifje
parser = do
  contents <- readFile "test.kuifje"
  print contents

  -- remove all "\n"
  let modifiedContents = removeNewlines contents
  let parsedProgram = P.parse parseKuifje "" modifiedContents
  case parsedProgram of
    Left err -> do
      error "The kuifje program contains syntax errors, Sukkel. :)"
    Right program -> do
      return program


