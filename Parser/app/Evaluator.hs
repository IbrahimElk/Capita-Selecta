module Evaluator (evaluate, Dist) where

import qualified Representation as Rp
import qualified Distributions as Ds
import qualified Control.Monad.State as S
import qualified Control.Monad.IO.Class as IO
import qualified Data.Ratio as DR
import qualified System.Random as R
import qualified Data.Map as M

import Data.Map.Strict ( elems, mapWithKey, singleton )
import GHC.Natural
import GHC.Real (fromRational)

type Prob = Rational
newtype Dist a = D { runD :: M.Map a Prob }
  deriving (Show)

-- type a ~> b = a -> Dist b
-- type Envs = String ~> Dist Int
type Env = M.Map String (Dist Int)

-- ------------------------------------
-- random function : 
-- ------------------------------------

contDist :: Rp.ContDist -> Dist Int
contDist _ =  hardcodedContDist

hardcodedContDist :: Dist Int
hardcodedContDist = D $ M.fromList
    [ (x, p) | x <- [0..19]
             , let p = if even x then 1 DR.% 20 else 3 DR.% 100
    ]

dscrDist :: Rp.DscrDist -> Dist Bool
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

evalExpr :: Rp.Expr -> S.State Env (Dist Int)
evalExpr (Rp.Add u v d) = do
  x <- evalExpr u
  y <- evalExpr v
  let z = sumDistributions x y
  let l = contDist d
  return (addPerturbation z l)  --additive noise added

evalExpr (Rp.Sub u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = subDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Rp.Mul u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = mulDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Rp.Div u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = divDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Rp.Mod u v d) = do
  env <- S.get
  x <- evalExpr u
  y <- evalExpr v
  let z = modDistributions x y
  let l = contDist d
  return (addPerturbation z l)

evalExpr (Rp.Lit u d) = do
  let l = contDist d
  return (addPerturbation (point u) l)

evalExpr (Rp.Var u) = do
    env <- S.get
    case env M.!? u of
        Just x  -> return x
        Nothing -> error $ "Variable " ++ u ++ " is not initialised!"

-- ---------------------------------------------------
-- ---------------------------------------------------

evalStatement :: Rp.Statement -> S.State Env ()
evalStatement (Rp.Stat var expr) = do
    env <- S.get
    resultDist <- evalExpr expr
    let updatedEnv = M.insert var resultDist env
    S.put updatedEnv

-- ---------------------------------------------------
-- ---------------------------------------------------

-- moet bool teruggeven want the condition kan ook van nested nature zijn!!!
evalComp :: Rp.Comp -> S.State Env (Dist Bool)
evalComp (Rp.Equal e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2
  let z       = equalDistributions resultDist1 resultDist2
  let l       = dscrDist d
  return (addBoolPerturbation z l)

evalComp (Rp.NotEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool = notEqualDistributions resultDist1 resultDist2
  let l        = dscrDist d
  return (addBoolPerturbation distBool l)

evalComp (Rp.LessThan e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = lessThanDistributions resultDist1 resultDist2
  let l       = dscrDist d
  let resultDist  = addBoolPerturbation distBool l
  return resultDist

evalComp (Rp.GreaterThan e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = greaterThanDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

evalComp (Rp.LessThanOrEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = lessThanOrEqualDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

evalComp (Rp.GreaterThanOrEqual e1 e2 d) = do
  resultDist1 <- evalExpr e1
  resultDist2 <- evalExpr e2

  let distBool    = greaterThanOrEqualDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

-- ---------------------------------------------------
-- ---------------------------------------------------

evalCond :: Rp.Cond -> S.State Env (Dist Bool)
evalCond (Rp.And e1 e2 d) = do
  resultDist1 <- evalCond e1
  resultDist2 <- evalCond e2

  let distBool    = andDistributions resultDist1 resultDist2
  let l           = dscrDist d
  let resultDist  = addBoolPerturbation distBool l
  return resultDist

evalCond (Rp.Or e1 e2 d) = do
  resultDist1 <- evalCond e1
  resultDist2 <- evalCond e2

  let distBool    = orDistributions resultDist1 resultDist2
  let l           = dscrDist d
  return $ addBoolPerturbation distBool l

evalCond (Rp.Comp c) = do evalComp c

-- ---------------------------------------------------
-- ---------------------------------------------------

-- opletten, runState vs evalState !!

evaluate :: Rp.Kuifje -> S.StateT Env IO (Dist Int)
evaluate Rp.Skip = do
  return (point 0)

evaluate (Rp.Return a) = do
  env <- S.get
  return (env M.! a)

-- is er geen beter manier om dit te doen? 
evaluate (Rp.Update statement restOfProgram) = do
  initialEnv <- S.get
  let result = S.runState (evalStatement statement) initialEnv
  S.put (snd result)
  updatedEnv <- S.get
  evaluate restOfProgram

evaluate (Rp.If condition trueBranch falseBranch restOfProgram) = do
  env <- S.get
  let resultDist = S.evalState (evalCond condition) env
  condResult <- S.liftIO $ sampleBoolDist resultDist
  if condResult
    then evaluate trueBranch
    else evaluate falseBranch
  evaluate restOfProgram

evaluate (Rp.While condition body restOfProgram) = do
  env <- S.get
  let resultDist = S.evalState (evalCond condition) env
  condResult <- S.liftIO $ sampleBoolDist resultDist
  if condResult
    then do
      evaluate body
      evaluate (Rp.While condition body restOfProgram)
    else
      evaluate restOfProgram

-- ---------------------------------------------------
-- ---------------------------------------------------

statement1 :: Rp.Statement
statement1 = Rp.Stat   "x" (Rp.Add (Rp.Lit 2 Ds.litDist) (Rp.Lit 1 Ds.litDist) Ds.addDist)

statement20 :: Rp.Cond
statement20 = Rp.Comp (Rp.Equal (Rp.Var "x") (Rp.Lit 3 Ds.litDist) Ds.boolDist)

statement21 :: Rp.Cond
statement21 = Rp.Comp (Rp.NotEqual (Rp.Var "x") (Rp.Lit 3 Ds.litDist) Ds.boolDist)

statement3 :: Rp.Statement
statement3 = Rp.Stat   "x" (Rp.Sub (Rp.Lit 3 Ds.litDist) (Rp.Lit 1 Ds.litDist) Ds.addDist)

statement4 :: Rp.Statement
statement4 = Rp.Stat   "t" (Rp.Mul (Rp.Var "x") (Rp.Lit 2 Ds.litDist) Ds.addDist)

statement5 :: Rp.Variable
statement5 = "x"

program1 :: Rp.Kuifje
program1
  = Rp.update statement1 <>  
    Rp.while statement20 (Rp.update statement3) (Rp.update statement4) <> 
    Rp.update statement1 <>
    Rp.cond statement21 (Rp.update statement3) (Rp.update statement4) (Rp.update statement1) <>
    Rp.returns statement5

program2 :: Rp.Kuifje
program2
  = Rp.update statement1

-- for development purposes: 
printDist :: Dist Int -> IO ()
printDist (D m) = mapM_ printEntry (M.toList m)
    where
        printEntry (x, p) = putStrLn $ show x ++ ": " ++ show (fromRational p :: Double)

main :: IO ()
main = do
  -- print program1 -- dit is wat je moet schrijven en ik zal dit parsen naar de Kuifje AST. 

  let initialEnv = M.empty
  let r = S.runStateT (evaluate program1) initialEnv
  d <- r
  putStrLn "Nice"
  print (snd d)

  -- putStrLn "result"
  -- print (fst d)
