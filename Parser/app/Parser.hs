{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser where

import Evaluator (evaluate)
import qualified Representation as Rp
import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)
import qualified Distributions as Ds
import qualified Data.Char as C 

-- -----------------------------------------
-- IMPLEMENTATIE VAN EEN PARSER:
-- -----------------------------------------

-- import Data.List.Utils (replace)
-- we parsen kuifje programma in haskell.
-- de parser programma laten runnen maar zo met args 
-- bv. parse somefile.kuifje --seed=123
-- seed is dan gebruikt voor random number generation.

-- FIXME : aslt mogelijk is om dat eerst uit een file te lezen, zou beter zijn. 
-- source code example.
-- serialisedProgram :: String
-- serialisedProgram = 
-- x = (2+1);
-- while ((( x == 3 ))){
-- x = (3-1);
-- }
-- t = (x*2);
-- return x;


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
parseComp = P.try parseEqual -- P.<|> P.try parseNotEqual 
  -- P.<|> P.try parseLessThan P.<|> P.try parseGreaterThan 
  -- P.<|> P.try parseLessThanOrEqual P.<|> P.try parseGreaterThanOrEqual

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
parseCond = parseCompCond -- P.<|> parseAndCond P.<|> parseOrCond

parseCompCond :: PS.Parser Rp.Cond
parseCompCond = do
  c <- parseComp
  return (Rp.Comp c)

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
  -- let message = "Parsed update: "  ++ show s
  -- trace message $ 
  P.spaces
  k <- parseKuifje
  -- let message = "Parsed update: "  ++ show s ++ "\n parsed kuifje of update: " ++ show k
  -- trace message $ 
  return (Rp.Update s k)

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
  P.char '{'
  k2 <- parseKuifje
  P.char '}'
  P.spaces
  k3 <- parseKuifje
  return (Rp.If c k1 k2 k3)

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
  k2 <- parseKuifje
  return (Rp.While c k1 k2)

removeNewlines :: String -> String
removeNewlines = filter (/= '\n')

main :: IO ()
main = do
  contents <- readFile "test.txt"
  -- remove all "\n" from contents. 
  print contents
  let modifiedContents = removeNewlines contents
  -- let modifiedContents = "x = (2+1);while ((( x == 3 ))){x = (3-1);}t = (x*2);return x;"
  print "begin"
  print modifiedContents
  let parsedProgram = P.parse parseKuifje "" modifiedContents
  print "end"
  case parsedProgram of
    Left err -> do 
      print "error"
      print err 
    Right program -> do 
      print "program"
      print program
  

