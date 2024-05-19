{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where
-- import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as S
import qualified Control.Monad.IO.Class as IO
import qualified Data.Ratio as DR
import qualified System.Random as R
import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS

import Data.Map.Strict ( elems, mapWithKey, singleton )
import GHC.Natural
import GHC.Real (fromRational)

type Prob = Rational
data Dist a = D { runD :: M.Map a Prob }
  deriving (Show)

type a ~> b = a -> Dist b
-- type Envs = String ~> Dist Int
type Env = M.Map String (Dist Int)

-- TODO: werk aan de parser. 
-- TOOD: werk aan de evaluator. 
-- (gebruik voor nu de 'mean' van de distributie ipv samplen.)

type Var = String
data Statement = Stat Var Expr

instance Show Statement where
    show (Stat s k1) = s ++ " = " ++ show k1 ++ ";"


data Expr = Add   Expr Expr ContDist
          | Sub   Expr Expr ContDist
          | Mul   Expr Expr ContDist
          | Div   Expr Expr ContDist
          | Mod   Expr Expr ContDist -- modulo operator. 
          | Lit   Int ContDist
          | Var   Var -- eerder gedeclareerde variablen kunnen hier gebruikt worden.

instance Show Expr where
  show (Add   e1 e2 c)  = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Sub   e1 e2 c)  = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
  show (Mul   e1 e2 c)  = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
  show (Div   e1 e2 c)  = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (Mod   e1 e2 c)  = "(" ++ show e1 ++ "%" ++ show e2 ++ ")"
  show (Lit   i c)      = show i
  show (Var   v)        = v

data Cond = And Cond Cond DscrDist
          | Or  Cond Cond DscrDist
          | Comp Comp

instance Show Cond where
  show (And  c1 c2 d)               = "(" ++ show c1 ++ " && "  ++ show c2  ++ ")"
  show (Or   c1 c2 d)               = "(" ++ show c1 ++ " || "  ++ show c2  ++ ")"
  show (Comp c      )               = "(" ++ show c  ++ ")"

data Comp = Equal        Expr Expr DscrDist
          | NotEqual     Expr Expr DscrDist
          | LessThan     Expr Expr DscrDist
          | GreaterThan  Expr Expr DscrDist
          | LessThanOrEqual    Expr Expr DscrDist
          | GreaterThanOrEqual Expr Expr DscrDist

instance Show Comp where
  show (Equal        e1 e2 d)       = "( " ++ show e1 ++ " == "  ++ show e2  ++ " )"
  show (NotEqual     e1 e2 d)       = "( " ++ show e1 ++ " != "  ++ show e2  ++ " )"
  show (LessThan     e1 e2 d)       = "( " ++ show e1 ++ " < "   ++ show e2  ++ " )"
  show (GreaterThan  e1 e2 d)       = "( " ++ show e1 ++ " > "   ++ show e2  ++ " )"
  show (LessThanOrEqual    e1 e2 d) = "( " ++ show e1 ++ " <= "  ++ show e2  ++ " )"
  show (GreaterThanOrEqual e1 e2 d) = "( " ++ show e1 ++ " >= "  ++ show e2  ++ " )"

type Mu     = Double
type Sigma  = Double
type Alpha  = Double
type Beta   = Double
type UpperBound = Double
type LowerBound = Double

data ContDist = Beta      Alpha Beta
              | Gaussian  Mu    Sigma
              | Uniform     LowerBound UpperBound
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
  | Return  Var
  | Update  Statement Kuifje
  | If      Cond Kuifje Kuifje Kuifje
  | While   Cond Kuifje Kuifje

instance Show Kuifje where
  show  Skip                = ""
  show (Return  a        )  = "Return " ++ a ++ ";\n"
  show (Update s k1      )  = show s ++ "\n" ++ show k1
  show (If     c k1 k2 k3)  = "if (" ++ show c ++ "){\n" ++
                                  show k1 ++ "}\n" ++ 
                              "else (" ++ show k2 ++ "){\n" ++
                                  show k3 ++ "}\n" 

  show (While  c k1 k2   )  = "while (" ++ show c ++ "){\n" ++ 
                                  show k1 ++ "}\n" ++ 
                                  show k2
instance Semigroup Kuifje where
  Return a      <> l    = Return a -- FIXME? 
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

returns :: Var -> Kuifje
returns = Return
-- ------------------------------------
-- random function : 
-- ------------------------------------

contDist :: ContDist -> Dist Int
contDist _ =  hardcodedContDist

hardcodedContDist :: Dist Int
hardcodedContDist = D $ M.fromList
    [ (x, p) | x <- [0..19]
             , let p = if even x then 1 DR.% 20 else 3 DR.% 100
    ]

dscrDist :: DscrDist -> Dist Bool
dscrDist _ =  hardcodedDscrDist

hardcodedDscrDist :: Dist Bool
hardcodedDscrDist = D $ M.fromList [(True, 1 DR.% 2), (False, 1 DR.% 2)]

-- ------------------------------------
-- evaluator function : 
-- ------------------------------------
point :: (Ord a) => a -> Dist a
point x = D $ singleton x 1

sampleBoolDist :: Dist Bool -> IO Bool
sampleBoolDist (D dist) = do
    let p = dist M.! True
    randomNumber <- generateRandomDouble
    return (randomNumber <= fromRational p)

generateRandomDouble :: IO Double
generateRandomDouble = R.randomRIO (0, 1)

-- ---------------------------------------------------
-- ---------------------------------------------------

sumDistributions :: Dist Int -> Dist Int -> Dist Int
sumDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
          [ (x+y, p1*p2)
          | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2 ]

subDistributions :: Dist Int -> Dist Int -> Dist Int
subDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
          [ (x+y, p1*p2)
          | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2 ]

mulDistributions :: Dist Int -> Dist Int -> Dist Int
mulDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
          [ (x*y, p1*p2)
          | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2 ]

divDistributions :: Dist Int -> Dist Int -> Dist Int
divDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
          [ (x `div` y, p1 * p2)
          | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2, y /= 0 ]

modDistributions :: Dist Int -> Dist Int -> Dist Int
modDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
            [ (x `mod` y, p1 * p2)
            | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2, y /= 0 ]

-- ---------------------------------------------------
-- ---------------------------------------------------

equalDistributions :: Dist Int -> Dist Int -> Dist Bool
equalDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
        [ (x == y, p1 * p2)
        | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

notEqualDistributions :: Dist Int -> Dist Int -> Dist Bool
notEqualDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x /= y, p1 * p2)
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

lessThanDistributions :: Dist Int -> Dist Int -> Dist Bool
lessThanDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x < y, p1 * p2) -- is goed idee ? voor elk pair van beide distributies??
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

greaterThanDistributions :: Dist Int -> Dist Int -> Dist Bool
greaterThanDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x > y, p1 * p2)
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

lessThanOrEqualDistributions :: Dist Int -> Dist Int -> Dist Bool
lessThanOrEqualDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x <= y, p1 * p2)
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

greaterThanOrEqualDistributions :: Dist Int -> Dist Int -> Dist Bool
greaterThanOrEqualDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x > y, p1 * p2)
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

andDistributions :: Dist Bool -> Dist Bool -> Dist Bool
andDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x && y, p1 * p2)
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

orDistributions :: Dist Bool -> Dist Bool -> Dist Bool
orDistributions (D dist1) (D dist2) =
    D $ M.fromListWith (+)
    [ (x || y, p1 * p2)
    | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2]

-- ---------------------------------------------------
-- ---------------------------------------------------

addPerturbation :: Dist Int -> Dist Int -> Dist Int
addPerturbation (D dist) (D noise) = D $ M.fromListWith (+) [ (x+n, p*q) | (x, p) <- M.toList dist, (n, q) <- M.toList noise ]

addBoolPerturbation :: Dist Bool -> Dist Bool -> Dist Bool
addBoolPerturbation (D dist) (D noise) = D $ M.fromListWith (+) [(x /= n, p * q) | (x, p) <- M.toList dist, (n, q) <- M.toList noise ]

-- ---------------------------------------------------
-- ---------------------------------------------------

evalExpr :: Expr -> S.State Env (Dist Int)
evalExpr (Add u v d) = do
  x <- evalExpr u
  y <- evalExpr v
  let z = sumDistributions x y
  let l = contDist d
  return (addPerturbation z l)  --additive noise added

evalExpr (Sub u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = subDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Mul u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = mulDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Div u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = divDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Mod u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = modDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Lit u d) = do
  let l = contDist d
  return (addPerturbation (point u) l)

evalExpr (Var u) = do
    env <- S.get
    case env M.!? u of
        Just x  -> return x
        Nothing -> error $ "Variable " ++ u ++ " is not initialised!"

-- ---------------------------------------------------
-- ---------------------------------------------------

evalStatement :: Statement -> S.State Env ()
evalStatement (Stat var expr) = do
    env <- S.get
    resultDist <- evalExpr expr
    let updatedEnv = M.insert var resultDist env
    S.put updatedEnv

-- ---------------------------------------------------
-- ---------------------------------------------------

-- moet bool teruggeven want the condition kan ook van nested nature zijn!!!
evalComp :: Comp -> S.State Env (Dist Bool)
evalComp (Equal e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2
  let z       = equalDistributions resultDist1 resultDist2
  let l       = dscrDist d
  return (addBoolPerturbation z l)

evalComp (NotEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool = notEqualDistributions resultDist1 resultDist2
  let l        = dscrDist d
  return (addBoolPerturbation distBool l)

evalComp (LessThan e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = lessThanDistributions resultDist1 resultDist2
  let l       = dscrDist d
  let resultDist  = addBoolPerturbation distBool l
  return resultDist

evalComp (GreaterThan e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = greaterThanDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

evalComp (LessThanOrEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = lessThanOrEqualDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

evalComp (GreaterThanOrEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = greaterThanOrEqualDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

-- ---------------------------------------------------
-- ---------------------------------------------------

evalCond :: Cond -> S.State Env (Dist Bool)
evalCond (And e1 e2 d) = do
  resultDist1 <- evalCond e1
  resultDist2 <- evalCond e2

  let distBool    = andDistributions resultDist1 resultDist2
  let l           = dscrDist d
  let resultDist  = addBoolPerturbation distBool l
  return resultDist

evalCond (Or e1 e2 d) = do
  resultDist1 <- evalCond e1
  resultDist2 <- evalCond e2

  let distBool    = orDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

evalCond (Comp c) = do evalComp c

-- ---------------------------------------------------
-- ---------------------------------------------------

evaluate :: Kuifje -> S.StateT Env IO (Dist Int)
evaluate (Return a) = do
  env <- S.get
  return (env M.! a)

evaluate (Update statement restOfProgram) = do
  env <- S.get
  let resultInt = S.evalState (evalStatement statement) env
  evaluate restOfProgram

evaluate (If condition trueBranch falseBranch restOfProgram) = do
  env <- S.get
  let resultDist = S.evalState (evalCond condition) env
  condResult <- S.liftIO $ sampleBoolDist resultDist
  if condResult
    then evaluate trueBranch
    else evaluate falseBranch
  evaluate restOfProgram

evaluate (While condition body restOfProgram) = do
  env <- S.get
  let resultDist = S.evalState (evalCond condition) env
  condResult <- S.liftIO $ sampleBoolDist resultDist
  if condResult
    then do
      evaluate body
      evaluate (While condition body restOfProgram)
    else
      evaluate restOfProgram

-- ---------------------------------------------------
-- ---------------------------------------------------

statement1 :: Statement
statement1 = Stat   "x" (Add (Lit 2 (Gaussian 0 1)) (Lit 1 (Gaussian 0 1)) (Gaussian 0 1))

statement2 :: Cond
statement2 = Comp (Equal (Var "x") (Lit 3 (Gaussian 0 1)) (Bernoulli 0))

statement3 :: Statement
statement3 = Stat   "x" (Sub (Lit 3 (Gaussian 0 1)) (Lit 1 (Gaussian 0 1)) (Gaussian 0 1))

statement4 :: Statement
statement4 = Stat   "t" (Mul (Var "x") (Lit 2 (Gaussian 0 1)) (Gaussian 0 1))

statement5 :: Var
statement5 = "x"

program1 :: Kuifje
program1
  = update statement1 <> while statement2 (update statement3) (update statement4) <> returns statement5

program2 :: Kuifje
program2
  = update statement1

-- serialisedProgram :: String
-- serialisedProgram = "Update statement1 (While statement2 (Update statement3 (Update statement4 (Return))))"

-- for development purposes: 
printDist :: Dist Int -> IO ()
printDist (D m) = mapM_ printEntry (M.toList m)
    where
        printEntry (x, p) = putStrLn $ show x ++ ": " ++ show (fromRational p :: Double)

-- main :: IO ()
-- main = do
--   let initialEnv = M.empty
--   result <- S.runStateT (evaluate program2) initialEnv

--   putStrLn "Dist Int:"
--   printDist (fst result)

--   putStrLn "\nEnv:"
--   print (snd result)

-- initialEnv :: Env
-- initialEnv = M.singleton "Void" (D $ M.singleton 0 (1 DR.% 1))


main :: IO ()
main = do
  print program1

  let initialEnv = M.empty
  result <- S.runStateT (evaluate program1) initialEnv

  putStrLn "\nEnv:"
  print (snd result)









