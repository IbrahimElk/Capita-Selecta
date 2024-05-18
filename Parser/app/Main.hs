{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where
import qualified Control.Monad.State as S
import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS
import Data.Map.Strict ( elems, mapWithKey, singleton )
import GHC.Natural

type Prob = Rational
newtype Dist a = D { runD :: M.Map a Prob }
type a ~> b = a -> Dist b

-- TODO: werk aan de parser. 
-- TOOD: werk aan de evaluator. 
-- (gebruik voor nu de 'mean' van de distributie ipv samplen.)


type Var = String
data Statement = Stat Var Expr
  deriving (Show)

data Expr = Add   Expr Expr ContDist
          | Sub   Expr Expr ContDist
          | Mul   Expr Expr ContDist
          | Div   Expr Expr ContDist
          | Mod   Expr Expr ContDist -- modulo operator. 
          | Lit   Int ContDist
          | Var   Var -- eerder gedeclareerde variablen kunnen hier gebruikt worden.

instance Show Expr where
  show (Add   e1 e2 c)  = "[ " ++ show e1 ++ " + " ++ show e2 ++ " CDist " ++ show c ++ " ] "
  show (Sub   e1 e2 c)  = "[ " ++ show e1 ++ " - " ++ show e2 ++ " CDist " ++ show c ++ " ] "
  show (Mul   e1 e2 c)  = "[ " ++ show e1 ++ " * " ++ show e2 ++ " CDist " ++ show c ++ " ]"
  show (Div   e1 e2 c)  = "[ " ++ show e1 ++ " / " ++ show e2 ++ " CDist " ++ show c ++ " ]"
  show (Mod   e1 e2 c)  = "[ " ++ show e1 ++ " % " ++ show e2 ++ " CDist " ++ show c ++ " ]"
  show (Lit   i c)      = "[ Lit " ++ show i ++ " CDist " ++ show c ++ " ]"
  show (Var   v)        = "[ Var"  ++ show v  ++ " ]"

data Cond = Equal        Expr Expr DscrDist
          | NotEqual     Expr Expr DscrDist
          | LessThan     Expr Expr DscrDist
          | GreaterThan  Expr Expr DscrDist
          | LessThanOrEqual    Expr Expr DscrDist
          | GreaterThanOrEqual Expr Expr DscrDist
          | And  Cond Cond DscrDist
          | Or   Cond Cond DscrDist

instance Show Cond where
  show (Equal        e1 e2 d)       = "( " ++ show e1 ++ " == "  ++ show e2  ++ " )"
  show (NotEqual     e1 e2 d)       = "( " ++ show e1 ++ " != "  ++ show e2  ++ " )"
  show (LessThan     e1 e2 d)       = "( " ++ show e1 ++ " < "   ++ show e2  ++ " )"
  show (GreaterThan  e1 e2 d)       = "( " ++ show e1 ++ " > "   ++ show e2  ++ " )"
  show (LessThanOrEqual    e1 e2 d) = "( " ++ show e1 ++ " <= "  ++ show e2  ++ " )"
  show (GreaterThanOrEqual e1 e2 d) = "( " ++ show e1 ++ " >= "  ++ show e2  ++ " )"
  show (And  c1 c2 d)               = "( " ++ show c1 ++ " && "  ++ show c2  ++ " )"
  show (Or   c1 c2 d)               = "( " ++ show c1 ++ " || "  ++ show c2  ++ " )"

type Mu     = Double
type Sigma  = Double
type Alpha  = Double
type Beta   = Double

data ContDist = Beta      Alpha Beta
              | Gaussian  Mu    Sigma
              | Uniform     Double Double
              | Exponential Double
              | Poisson     Double

instance Show ContDist where
  show (Beta     a b    ) = "Beta "         ++ show a ++ " " ++ show b
  show (Gaussian m s    ) = "Gaussian "     ++ show m ++ " " ++ show s
  show (Uniform  a b    ) = "Beta "         ++ show a ++ " " ++ show b
  show (Exponential lmbd) = "Exponential "  ++ show lmbd
  show (Poisson r       ) = "Poisson "      ++ show r

data DscrDist = Bernoulli   Double

instance Show DscrDist where
  show (Bernoulli p) = "Bernoulli " ++ show p


data Kuifje
  = Skip
  | Update  Statement Kuifje
  | If      Cond Kuifje Kuifje Kuifje
  | While   Cond Kuifje Kuifje

instance Show Kuifje where
  show Skip                 = "Skip"
  show (Update s k1      )  = "Update " ++ show s ++ "(" ++ show k1 ++ ")"
  show (If     c k1 k2 k3)  = "If "     ++ show c ++ "(" ++ show k1 ++ ")" ++ "(" ++ show k2 ++ ")"  ++ "(" ++ show k3 ++ ")"
  show (While  c k1 k2   )  = "While "  ++ show c ++ "(" ++ show k2 ++ ")" ++ "(" ++ show k2 ++ ")"

instance Semigroup Kuifje where
  Skip          <> l    = l
  Update s p    <> l    = Update s (p <> l)
  While c p q   <> l    = While c p (q <> l)
  If c p q r    <> l    = If c p q (r <> l)

instance Monoid Kuifje where
  mempty = Skip
  mappend = (<>)

skip :: Kuifje
skip = Skip

update :: Statement -> Kuifje
update f = Update f skip

while :: Cond -> Kuifje -> Kuifje -> Kuifje
while = While

cond :: Cond -> Kuifje -> Kuifje -> Kuifje -> Kuifje
cond = If

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
  return (Equal (Lit 5 (Uniform 2 2)) (Lit 5 (Uniform 2 2)) (Bernoulli 2))

skipParser :: PS.Parser Kuifje
skipParser = do
    P.string "Skip"
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

-- ------------------------------------
-- evaluator function : 
-- ------------------------------------

-- elk variable moet een distributie hebben. 
-- deze int moet vervangen worden naar een distributie.
-- geef me de Env van de State, de State houd een tuple bij, de state zelf, Env, en de output van de vorige operatie Int.

-- data Statement = Stat Var Expr
-- de uistkomst van deze functie is stochastic van aard, 
-- we voegen een distributie toe, 
-- maar we rekenen enkel verder met een random getal uit die distributie

-- das eig die keili teken ofzo, ~> ??
type Env = M.Map String (Dist Int)

-- newtype Dist a = D { runD :: M.Map a Prob }
-- extractInt :: Dist Int -> Int
-- extractInt (D x) = x

point :: (Ord a) => a -> Dist a
point x = D $ singleton x 1

createDistFromLit :: Int -> Dist Int
createDistFromLit  = point

extractRandom :: Dist Int -> Int
extractRandom (D dist) = fst $ M.foldrWithKey (\k p acc -> if p > snd acc then (k, p) else acc) (0, 0) dist

evalExpr :: Expr -> Env -> Dist Int
evalExpr 



evalStatement :: Statement -> S.State Env Int

-- verander de env de distributie van x wordt verandert naar die van y. (overwrite)
evalStatement (Stat x (Var y))   = do
    env <- S.get
    let distY = env M.! y

    -- M.insert : 
    -- Insert a new key and value in the map. 
    -- If the key is already present in the map, 
    -- the associated value is replaced with the supplied value.
    -- new key 'x' and new value 'distY' associated with key.
    -- insert into dictionary 'env'

    let updatedEnv = M.insert x distY env
    S.put updatedEnv
    let distInt = env M.! x
    return (extractRandom distInt)

-- voeg nieuwe distrubutie toe aan de env vanuitde literal.
evalStatement (Stat x (Lit y d))   = do
    env <- S.get
    let distX = createDistFromLit y d

    let updatedEnv = M.insert x distX env
    S.put updatedEnv

    return (extractRandom distX)

evalStatement (Stat x (Mod p q d))   = do
    env <- S.get
    r <- evalExpr p env
    -- geef me de distributie van deze expressie. 
    -- (env is meegegeven, maar env mag je niet veranderen, enkel gebruiken)

    s <- evalExpr q env


    let distX = createDistFromLit y d

    let updatedEnv = M.insert x distX env

    S.put updatedEnv

    return (extractRandom distX)


evalStatement (Add t u d) = do
    new_t <- evalExpr t
    new_u <- evalExpr u
    return (new_t + new_u)

evalStatement (Sub t u d) = do
    new_t <- evalExpr t
    new_u <- evalExpr u
    return (new_t - new_u)

evalStatement (Mul t u d) = do
    new_t <- evalExpr t
    new_u <- evalExpr u
    return (new_t * new_u)

evalStatement (Div t u d) = do
    new_t <- evalExpr t
    new_u <- evalExpr u
    return (new_t * new_u)


evalStatement (Mod t u d) = do
    new_t <- evalExpr t
    new_u <- evalExpr u
    return (new_t * new_u)

evalStatement (Lit t d) = do
    return (t)


-- data Kuifje
--   = Skip
--   | Update  Statement Kuifje
--   | If      Cond Kuifje Kuifje Kuifje
--   | While   Cond Kuifje Kuifje


-- TODO: moeilijke gedeelte ...

-- data Stack = { S :: x Int }
-- Stack -> Kuifje -> Int
evaluate :: Kuifje -> Int
evaluate x Skip = x

evaluate state (Update f next) = do
  let newDist = f state
      newState = sampleInt newDist
  evaluate newState next

evaluate state (If condStmt trueBranch falseBranch restOfProgram) = do
  let condDist = condStmt state
      condResult = sampleBool condDist
  if condResult
    then evaluate state trueBranch
    else evaluate state falseBranch

evaluate state (While condStmt body restOfProgram) = do
  let condDist = condStmt state
      condResult = sampleBool condDist
  if condResult
    then do
      let
        newState = evaluate state body
      evaluate newState (While condStmt body)
    else
      state

evaluate _ _ = error "nice"

-- point estimate for distribtuion.
average :: Dist Int -> Rational
average = sum . mapWithKey multiplyIntRational . runD

sampleInt :: Dist Int -> Int
sampleInt =  rationalToInteger . average

sampleBool :: Dist Bool -> Bool
sampleBool = intToBool . sampleInt . boolDistToIntDist

-- -------
-- Utils
-- -------

multiplyIntRational :: Int -> Rational -> Rational
multiplyIntRational int rational = toRational int * rational

rationalToInteger :: Rational -> Int
rationalToInteger = round

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

intToBool :: Int -> Bool
intToBool 1 = True
intToBool 0 = False
intToBool _ = error "nice"

boolDistToIntDist :: Dist Bool -> Dist Int
boolDistToIntDist (D boolDist) = D $ M.mapKeysMonotonic boolToInt boolDist

-- ------------------------------ 
-- EXAMPLE: 
-- ------------------------------

point :: (Ord a) => a -> Dist a
point x = D $ singleton x 1

-- later nog complexere distributies toevoegen. 
statement1 :: Int -> Dist Int
statement1 x = point (x + 1)

statement2 :: Int -> Dist Bool
statement2 x = point (x < 0)

statement3 :: Int -> Dist Int
statement3 x = point (x - 2)

statement4 :: Int -> Dist Int
statement4 x = point (x + 1)

program :: Kuifje
program
  = update' statement1 <>
    while   statement2 (
      update' statement3 <>
      update' statement4
    )

program' :: Kuifje
program' = Update statement1 (While statement2 (Update statement3 (Update statement4 (Skip))))

serialisedProgram :: String
serialisedProgram = "Update statement1 (While statement2 (Update statement3 (Update statement4 (Skip))))"

main :: IO ()
main = let
  objProgram = P.parse kuifjeParser "" serialisedProgram
  in case objProgram of
    Left  err   -> print err
    Right out   -> print (evaluate 0 out)

-- ---------------------------------
-- Calculator example
-- ---------------------------------

-- KAN DISTRIBUTIES NOG INTERSSANTER MAKEN.
-- context van calculator exmaple: 
-- soort van numerieke stabilieit visualisator, als x en y perturbaties bevatten, welke operaties heeft bredere distributies etc. 
-- conditionering van het probleem en numerieke stabiliteit etc. 
-- additive perturbaties

type Perturbation = Rational

-- probs zijn hardcoded en de perturbatie ook.
-- sws bestaat er al zo iets die x en y perturbeert en omzet naar dist, en dan gwn addDist gebruiken.
add :: Int -> Int -> Dist Int
add x y =  D $ M.fromList [(result, correctProb), (result+perturbation, incorrectProb), (result-perturbation, incorrectProb)]
  where
    result = x + y
    -- moet eig normale distributie error zijn. 
    perturbation = 5
    correctProb = 0.8
    -- incorrectProb = (1 - correctProb) / 2  
    incorrectProb = 0.1

addDist :: Dist Int -> Dist Int -> Dist Int
addDist (D distX) (D distY) = normalizeDist $ D $ M.fromListWith (+)
    [ (result, resultProb * probX * probY)
    | (x, probX) <- M.toList distX
    , (y, probY) <- M.toList distY
    , let resultDist = add x y
    , (result, resultProb) <- M.toList (runD resultDist)
    ]

normalizeDist :: Dist a -> Dist a
normalizeDist (D dist) = D $ M.map (/ totalProb) dist
  where
    totalProb = sum (M.elems dist)

sub :: Int -> Int -> Dist Int
sub x y =  D $ M.fromList [(result, correctProb), (result+perturbation, incorrectProb), (result-perturbation, incorrectProb)]
  where
    result = x - y
    -- moet eig normale distributie error zijn. 
    perturbation = 5
    correctProb = 0.8
    -- incorrectProb = (1 - correctProb) / 2  
    incorrectProb = 0.1

subDist :: Dist Int -> Dist Int -> Dist Int
subDist (D distX) (D distY) = normalizeDist $ D $ M.fromListWith (+)
    [ (result, resultProb * probX * probY)
    | (x, probX) <- M.toList distX
    , (y, probY) <- M.toList distY
    , let resultDist = sub x y
    , (result, resultProb) <- M.toList (runD resultDist)
    ]

mul :: Int -> Int -> Dist Int
mul x y =  D $ M.fromList [(result, correctProb), (result+perturbation, incorrectProb), (result-perturbation, incorrectProb)]
  where
    result = x * y
    perturbation = 5
    correctProb = 0.8
    -- incorrectProb = (1 - correctProb) / 2  
    incorrectProb = 0.1

mulDist :: Dist Int -> Dist Int -> Dist Int
mulDist (D distX) (D distY) = normalizeDist $ D $ M.fromListWith (+)
    [ (result, resultProb * probX * probY)
    | (x, probX) <- M.toList distX
    , (y, probY) <- M.toList distY
    , let resultDist = mul x y
    , (result, resultProb) <- M.toList (runD resultDist)
    ]
-- more patterns if one of the dist is empty.
-- vooral door die div
divide :: Int -> Int -> Dist Int
divide x y
  | y /= 0    = D $ M.fromList [(result, correctProb), (result+perturbation, incorrectProb), (result-perturbation, incorrectProb)]
  | otherwise = D M.empty
  where
    result = x `div` y
    perturbation = 5
    correctProb = 0.8
    incorrectProb = 0.1

divDist :: Dist Int -> Dist Int -> Dist Int
divDist (D distX) (D distY) = normalizeDist $ D $ M.fromListWith (+)
    [ (result, resultProb * probX * probY)
    | (x, probX) <- M.toList distX
    , (y, probY) <- M.toList distY
    , let resultDist = divide x y
    , (result, resultProb) <- M.toList (runD resultDist)
    ]

-- squareRoot :: Int -> Dist Float
-- squareRoot x = if x >= 0 then D $ singleton (sqrt $ fromIntegral x) 1 else D M.empty

-- power :: Int -> Int -> Dist Int
-- power x y = D $ singleton (x ^ y) 1

-- logarithm :: Int -> Dist Float
-- logarithm x = if x > 0 then D $ singleton (logBase 10 $ fromIntegral x) 1 else D M.empty 

-- extra functie die kunnen geimplementeerd worden in calculator, maar is moeilijk denk ik: 
-- -- TODO: (s ~> Bool)
-- isPrime :: Int -> Bool
-- isPrime n
--   | n <= 1 = False
--   | otherwise = all (\x -> n `mod` x /= 0) [2..intSquareRoot n]

-- intSquareRoot :: Int -> Int
-- intSquareRoot = round . sqrt . fromIntegral


generateHistogramTerminal :: Dist Int -> IO ()
generateHistogramTerminal (D outcomes) =
    putStrLn "Calculator Result Histogram" >>
    mapM_ (\(x, p) -> putStrLn (show x ++ ": " ++ replicate (round $ p * 100) '*')) (M.toList outcomes)

exampleDist1 :: Dist Int
exampleDist1 = D $ M.fromList [(1, 0.2), (2, 0.5), (3, 0.3)]

exampleDist2 :: Dist Int
exampleDist2 = D $ M.fromList [(0, 0.1), (1, 0.2), (2, 0.3), (3, 0.4)]

main2 :: IO ()
main2 = do
    putStrLn "Example 1:"
    generateHistogramTerminal exampleDist1
    -- putStrLn "\nExample 2:"
    -- generateHistogramFigure exampleDist2






-- IDEA: 

-- Choose a Plotting Library: Select a Haskell plotting library such as haskell-plot or Chart. 
-- These libraries provide functions and types for creating a wide range of plots, 
-- including histograms, line charts, scatter plots, and heatmaps.

-- extra ideen : 
-- Extend the Evaluator: Modify the evaluator function to compute additional statistical metrics or 
-- data that can be used for plotting. For example, you may calculate the mean, median, variance, or 
-- other summary statistics of the stochastic processes or probability distributions.

-- PROBLEEM2:
-- Hoe samplen van een Dist int type. 
-- blijkbaar kun je dat doen met de Dist monad.
-- sample :: Dist Int -> Int
-- of 
-- sample :: Dist Bool -> Bool
-- Mailtje sturen naar prof voor deze functie. 

-- quickCheck : 
-- 
-- 








