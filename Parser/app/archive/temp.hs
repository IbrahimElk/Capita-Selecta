{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import qualified Control.Monad.State as S
import qualified Data.Ratio as DR
import qualified System.Random as R
import qualified Data.Map as M
import Data.Map.Strict (elems, mapWithKey, singleton)
import GHC.Natural
import GHC.Real (fromRational)

type Prob = Rational
newtype Dist a = D { runD :: M.Map a Prob }
type a ~> b = a -> Dist b

type Var = String
data Statement = Stat Var Expr
  deriving (Show)

data Expr = Add   Expr Expr ContDist
          | Sub   Expr Expr ContDist
          | Mul   Expr Expr ContDist
          | Div   Expr Expr ContDist
          | Mod   Expr Expr ContDist
          | Lit   Int ContDist
          | Var   Var 

instance Show Expr where
  show (Add   e1 e2 c)  = "[ " ++ show e1 ++ " + " ++ show e2 ++ " CDist " ++ show c ++ " ] "
  show (Sub   e1 e2 c)  = "[ " ++ show e1 ++ " - " ++ show e2 ++ " CDist " ++ show c ++ " ] "
  show (Mul   e1 e2 c)  = "[ " ++ show e1 ++ " * " ++ show e2 ++ " CDist " ++ show c ++ " ]"
  show (Div   e1 e2 c)  = "[ " ++ show e1 ++ " / " ++ show e2 ++ " CDist " ++ show c ++ " ]"
  show (Mod   e1 e2 c)  = "[ " ++ show e1 ++ " % " ++ show e2 ++ " CDist " ++ show c ++ " ]"
  show (Lit   i c)      = "[ Lit " ++ show i ++ " CDist " ++ show c ++ " ]"
  show (Var   v)        = "[ Var"  ++ show v  ++ " ]"

data Cond = And Cond Cond DscrDist
          | Or  Cond Cond DscrDist
          | Comp Comp

instance Show Cond where
  show (And  c1 c2 d)               = "( " ++ show c1 ++ " && "  ++ show c2  ++ " )"
  show (Or   c1 c2 d)               = "( " ++ show c1 ++ " || "  ++ show c2  ++ " )"
  show (Comp c      )               = "( " ++ show c  ++ " )"

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
  show (Uniform  a b    ) = "Uniform "      ++ show a ++ " " ++ show b
  show (Exponential lmbd) = "Exponential "  ++ show lmbd
  show (Poisson r       ) = "Poisson "      ++ show r

data DscrDist = Bernoulli   Double

instance Show DscrDist where
  show (Bernoulli p) = "Bernoulli " ++ show p

data Kuifje
  = Return
  | Update  Statement Kuifje
  | If      Cond Kuifje Kuifje Kuifje
  | While   Cond Kuifje Kuifje

instance Show Kuifje where
  show Return                 = "Return"
  show (Update s k1      )  = "Update " ++ show s ++ "(" ++ show k1 ++ ")"
  show (If     c k1 k2 k3)  = "If "     ++ show c ++ "(" ++ show k1 ++ ")" ++ "(" ++ show k2 ++ ")"  ++ "(" ++ show k3 ++ ")"
  show (While  c k1 k2   )  = "While "  ++ show c ++ "(" ++ show k2 ++ ")" ++ "(" ++ show k2 ++ ")"

instance Semigroup Kuifje where
  Return        <> l    = l
  Update s p    <> l    = Update s (p <> l)
  While c p q   <> l    = While c p (q <> l)
  If c p q r    <> l    = If c p q (r <> l)

instance Monoid Kuifje where
  mempty = Return
  mappend = (<>)

skip :: Kuifje
skip = Return

update :: Statement -> Kuifje
update f = Update f skip

while :: Cond -> Kuifje -> Kuifje -> Kuifje
while = While

cond :: Cond -> Kuifje -> Kuifje -> Kuifje -> Kuifje
cond = If

type Env = M.Map String (Dist Int)

point :: (Ord a) => a -> Dist a
point x = D $ singleton x 1

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

doubleToRational :: Double -> Prob
doubleToRational = (DR.% 1) . floor . (* 1000)

sampleDist :: Dist Int -> IO Int
sampleDist (D dist) = do
    let cumDist = scanl1 (\(v1, p1) (v2, p2) -> (v2, p1 + p2)) (M.toList dist)
    r <- generateRandomDouble
    let rand = doubleToRational r
    return $ fst $ head $ dropWhile ((< rand) . snd) cumDist

sampleBoolDist :: Dist Bool -> IO Bool
sampleBoolDist (D dist) = do
    let p = dist M.! True
    r <- generateRandomDouble
    return (r <= fromRational p)

bernoulliTrial :: Double -> IO Bool
bernoulliTrial p = do
    r <- generateRandomDouble
    return (r <= p)

generateRandomDouble :: IO Double
generateRandomDouble = R.randomRIO (0, 1)

runExpr :: Expr -> S.StateT Env IO Int
runExpr (Lit n _)       = return n
runExpr (Var x)         = do
  env <- S.get
  let (D dist) = env M.! x
  S.liftIO $ sampleDist (D dist)
runExpr (Add e1 e2 d)   = do
  v1 <- runExpr e1
  v2 <- runExpr e2
  return (v1 + v2)
runExpr (Sub e1 e2 d)   = do
  v1 <- runExpr e1
  v2 <- runExpr e2
  return (v1 - v2)
runExpr (Mul e1 e2 d)   = do
  v1 <- runExpr e1
  v2 <- runExpr e2
  return (v1 * v2)
runExpr (Div e1 e2 d)   = do
  v1 <- runExpr e1
  v2 <- runExpr e2
  return (v1 `div` v2)
runExpr (Mod e1 e2 d)   = do
  v1 <- runExpr e1
  v2 <- runExpr e2
  return (v1 `mod` v2)

runComp :: Comp -> S.StateT Env IO Bool
runComp (Equal e1 e2 d)              = (==)   <$> runExpr e1 <*> runExpr e2
runComp (NotEqual e1 e2 d)           = (/=)   <$> runExpr e1 <*> runExpr e2
runComp (LessThan e1 e2 d)           = (<)    <$> runExpr e1 <*> runExpr e2
runComp (GreaterThan e1 e2 d)        = (>)    <$> runExpr e1 <*> runExpr e2
runComp (LessThanOrEqual e1 e2 d)    = (<=)   <$> runExpr e1 <*> runExpr e2
runComp (GreaterThanOrEqual e1 e2 d) = (>=)   <$> runExpr e1 <*> runExpr e2

runCond :: Cond -> S.StateT Env IO Bool
runCond (And c1 c2 d)    = do
  v1 <- runCond c1
  v2 <- runCond c2
  return (v1 && v2)
runCond (Or c1 c2 d)     = do
  v1 <- runCond c1
  v2 <- runCond c2
  return (v1 || v2)
runCond (Comp c)         = runComp c

runStatement :: Statement -> S.StateT Env IO ()
runStatement (Stat x e)  = do
  v <- runExpr e
  S.modify $ M.insert x (point v)

runKuifje :: Kuifje -> S.StateT Env IO ()
runKuifje Return                 = return ()
runKuifje (Update s p)           = runStatement s >> runKuifje p
runKuifje (If c p1 p2 k)         = do
  v <- runCond c
  if v then runKuifje p1 else runKuifje p2
  runKuifje k
runKuifje (While c p k)          = do
  v <- runCond c
  if v then runKuifje (p <> While c p k) else runKuifje k

eval :: Env -> Kuifje -> IO Env
eval env prog = S.execStateT (runKuifje prog) env

-- ------------------------------------
-- Example program : 
-- ------------------------------------

prog :: Kuifje
prog = update (Stat "x" (Lit 3 (Gaussian 0 1)))
    <> while (Comp (LessThan (Var "x") (Lit 10 (Uniform 0 1))) (Bernoulli 0.5)) loop skip
  where loop = update (Stat "x" (Add (Var "x") (Lit 1 (Poisson 0.5)) (Beta 1 2)))

main :: IO ()
main = do
  finalState <- eval M.empty prog
  print finalState
