module Parser.Src.Evaluator (
  module Parser.Src.Evaluator
) where

import Parser.Src.Representation
import qualified Parser.Src.Distributions as D

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State as S
import qualified System.Random as Ra
import qualified Data.Ratio as DR
import qualified Data.Map as M

import Data.Map.Strict ( elems, mapWithKey, singleton )
import GHC.Natural
import GHC.Real (fromRational)

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
generateRandomDouble = Ra.randomRIO (0, 1)

-- ---------------------------------------------------
-- ---------------------------------------------------

sumDistributions :: Dist Int -> Dist Int -> Dist Int
sumDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
          [ (x+y, p1*p2)
          | (x, p1) <- M.toList dist1, (y, p2) <- M.toList dist2 ]

subDistributions :: Dist Int -> Dist Int -> Dist Int
subDistributions (D dist1) (D dist2) = D $ M.fromListWith (+)
          [ (x-y, p1*p2)
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

normalizeBDistribution :: Dist Bool -> Dist Bool 
normalizeBDistribution (D dist) = D $ M.map (/ totalProb) dist
  where
    totalProb = sum (M.elems dist)

normalizeDistribution :: Dist Int -> Dist Int 
normalizeDistribution (D dist) = removeZeros $ D $ M.map (/ totalProb) dist
  where
    totalProb = sum (M.elems dist)

-- analoog aan reduction functie in Kuifje source code. 
removeZeros :: Dist Int -> Dist Int 
removeZeros (D dist) = D $ M.filter (/= 0) dist

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
  let nz = normalizeDistribution z
  let l = D.contDist d
  let resultDist = addPerturbation nz l --additive noise added
  return $ normalizeDistribution resultDist

evalExpr (Sub u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = subDistributions x y
  let nz = normalizeDistribution z
  let l = D.contDist d
  let resultDist = addPerturbation nz l
  return $ normalizeDistribution resultDist

evalExpr (Mul u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = mulDistributions x y
  let nz = normalizeDistribution z
  let l = D.contDist d
  let resultDist = addPerturbation nz l
  return $ normalizeDistribution resultDist

evalExpr (Div u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = divDistributions x y
  let nz = normalizeDistribution z
  let l = D.contDist d
  let resultDist = addPerturbation nz l
  return $ normalizeDistribution resultDist

evalExpr (Mod u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = modDistributions x y
  let nz = normalizeDistribution z
  let l = D.contDist d
  let resultDist = addPerturbation nz l
  return $ normalizeDistribution resultDist

evalExpr (Lit u d) = do
  let l = D.contDist d
  return (normalizeDistribution $ addPerturbation (point u) l)

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
  let distBool       = equalDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  let l       = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalComp (NotEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool = notEqualDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  let l        = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalComp (LessThan e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = lessThanDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  let l       = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalComp (GreaterThan e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = greaterThanDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  let l           = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalComp (LessThanOrEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = lessThanOrEqualDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  let l           = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalComp (GreaterThanOrEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = greaterThanOrEqualDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  let l           = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)
-- ---------------------------------------------------
-- ---------------------------------------------------

evalCond :: Cond -> S.State Env (Dist Bool)
evalCond (And e1 e2 d) = do
  resultDist1 <- evalCond e1
  resultDist2 <- evalCond e2

  let distBool    = andDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool

  let l           = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalCond (Or e1 e2 d) = do
  resultDist1 <- evalCond e1
  resultDist2 <- evalCond e2

  let distBool    = orDistributions resultDist1 resultDist2
  let nz = normalizeBDistribution distBool
  
  let l           = D.dscrDist d
  let resultDist  = addBoolPerturbation nz l
  return (normalizeBDistribution resultDist)

evalCond (Comp c) = do evalComp c

-- ---------------------------------------------------
-- ---------------------------------------------------

-- opletten, runState vs evalState !!

evaluate :: Kuifje -> S.StateT Env IO (Dist Int)
evaluate Skip = do
  return (point 0)

evaluate (Return a) = do
  env <- S.get
  return (env M.! a)

-- is er geen beter manier om dit te doen? 
evaluate (Update statement restOfProgram) = do
  initialEnv <- S.get
  let result = S.runState (evalStatement statement) initialEnv
  S.put (snd result)
  updatedEnv <- S.get
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
