module Parserator (
  module Parserator
) where
import qualified Text.Parsec.String as PS
import Representation
import qualified Distributions as Ds
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

digit :: PS.Parser Expr
digit = do
  x <- token (P.satisfy C.isDigit)
  return (Lit (C.ord x - C.ord '0') Ds.litDist)

variable :: PS.Parser Expr
variable = do
  x <- P.many1 P.letter
  return (Var x)

parseVar :: PS.Parser String
parseVar = P.many1 P.letter

symb :: String -> PS.Parser String
symb w = token (P.string w)

-- -------------------------------------------------------------
-- -------------------------------------------------------------

addop :: PS.Parser (Expr -> Expr -> Expr)
addop = do {symb "+"; return (\x y -> Add x y Ds.addDist)}

subop :: PS.Parser (Expr -> Expr -> Expr)
subop = do {symb "-"; return (\x y -> Sub x y Ds.subDist)}

addsubop :: PS.Parser (Expr -> Expr -> Expr)
addsubop =  addop P.<|> subop

-- -------------------------------------------------------------
-- -------------------------------------------------------------

mulop :: PS.Parser (Expr -> Expr -> Expr)
mulop = do {symb "*"; return (\x y -> Mul x y Ds.mulDist)}

divop :: PS.Parser (Expr -> Expr -> Expr)
divop = do {symb "/"; return (\x y -> Div x y Ds.divDist)}

modop :: PS.Parser (Expr -> Expr -> Expr)
modop = do {symb "%"; return (\x y -> Mod x y Ds.divDist)}

muldivop :: PS.Parser (Expr -> Expr -> Expr)
muldivop = mulop P.<|> divop P.<|> modop

-- -------------------------------------------------------------
-- -------------------------------------------------------------

factor :: PS.Parser Expr
factor = digit P.<|> variable P.<|> do {symb "("; n <- parseExpr ; symb ")"; return n}

term :: PS.Parser Expr
term = factor `P.chainl1` muldivop

parseExpr :: PS.Parser Expr
parseExpr = term `P.chainl1` addsubop

-- -------------------------------------------------------
-- -------------------------------------------------------
--          KUIFJE LANGUAGE PARSER
-- -------------------------------------------------------
-- -------------------------------------------------------

parseComp :: PS.Parser Comp
parseComp = P.try parseEqual  P.<|> P.try parseNotEqual
  P.<|> P.try parseLessThan P.<|> P.try parseGreaterThan
  P.<|> P.try parseLessThanOrEqual P.<|> P.try parseGreaterThanOrEqual

parseEqual :: PS.Parser Comp
parseEqual = do
  P.spaces
  e1 <- parseExpr
  P.spaces
  P.string "=="
  P.spaces
  e2 <- parseExpr
  P.spaces
  return (Equal e1 e2 Ds.boolDist)

parseNotEqual :: PS.Parser Comp
parseNotEqual = do
  e1 <- parseExpr
  P.spaces
  P.string "!="
  P.spaces
  e2 <- parseExpr
  return (NotEqual e1 e2 Ds.boolDist)

parseLessThan :: PS.Parser Comp
parseLessThan = do
  e1 <- parseExpr
  P.spaces
  P.char '<'
  P.spaces
  e2 <- parseExpr
  return (LessThan e1 e2 Ds.boolDist)

parseGreaterThan :: PS.Parser Comp
parseGreaterThan = do
  e1 <- parseExpr
  P.spaces
  P.char '>'
  P.spaces
  e2 <- parseExpr
  return (GreaterThan e1 e2 Ds.boolDist)

parseLessThanOrEqual :: PS.Parser Comp
parseLessThanOrEqual = do
  e1 <- parseExpr
  P.spaces
  P.string "<="
  P.spaces
  e2 <- parseExpr
  return (LessThanOrEqual e1 e2 Ds.boolDist)

parseGreaterThanOrEqual :: PS.Parser Comp
parseGreaterThanOrEqual = do
  e1 <- parseExpr
  P.spaces
  P.string ">="
  P.spaces
  e2 <- parseExpr
  return (GreaterThanOrEqual e1 e2 Ds.boolDist)

parseCond :: PS.Parser Cond
parseCond = parseCompCond P.<|> parseAndCond P.<|> parseOrCond

parseCompCond :: PS.Parser Cond
parseCompCond = do
  Comp <$> parseComp

parseAndCond :: PS.Parser Cond
parseAndCond = do
  c1 <- parseCond
  P.spaces
  P.string "&&"
  P.spaces
  c2 <- parseCond
  return (And c1 c2 Ds.boolDist)

parseOrCond :: PS.Parser Cond
parseOrCond = do
  c1 <- parseCond
  P.spaces
  P.string "||"
  P.spaces
  c2 <- parseCond
  return (Or c1 c2 Ds.boolDist)

parseStatement :: PS.Parser Statement
parseStatement = do
  v <- parseVar
  P.spaces
  P.char '='
  P.spaces
  e <- parseExpr
  P.char ';'
  -- let message = "Parsed Var: " ++ show v  ++ " Parsed Expr: " ++ show e
  -- trace message $ return (Stat "x" (Lit 2 Ds.litDist))
  return (Stat v e)

parseKuifje :: PS.Parser Kuifje
parseKuifje = parseIf P.<|> parseWhile P.<|> parseReturn P.<|> parseUpdate P.<|> parseSkip

parseSkip :: PS.Parser Kuifje
parseSkip = do
  return Skip

parseReturn :: PS.Parser Kuifje
parseReturn = do
  P.string "return"
  P.spaces
  v <- parseVar
  P.char ';'
  return (Return v)

parseUpdate :: PS.Parser Kuifje
parseUpdate = do
  s <- parseStatement
  P.spaces
  Update s <$> parseKuifje

parseIf :: PS.Parser Kuifje
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
  If c k1 k2 <$> parseKuifje

parseWhile :: PS.Parser Kuifje
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
  While c k1 <$> parseKuifje

removeNewlines :: String -> String
removeNewlines = filter (/= '\n')

type FileName = String

parser :: FileName -> IO Kuifje
parser filename = do
  contents <- readFile filename
  let modifiedContents = removeNewlines contents
  let parsedProgram = P.parse parseKuifje "" modifiedContents
  case parsedProgram of
    Left err -> do
      error "The kuifje program contains syntax errors, Sukkel. :)"
    Right program -> do
      return program


-- statement20 :: Comp
-- statement20 = Equal (Var "x") (Lit 3 Ds.litDist) Ds.boolDist

-- main :: IO()
-- main = do 
--   let parsedCond = P.parse parseEqual "" "x == 2"
--   case parsedCond of
--     Left err -> print err
--     Right actual1 -> print actual1