{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Control.Monad.State as S
import qualified Data.Map as M

newtype Dist a = D { runD :: M.Map a Rational}

type Env = M.Map String (Dist Int)

type Var = String
data Statement = Stat Var Expr
  deriving (Show)


type Mu     = Double
type Sigma  = Double
type Alpha  = Double
type Beta   = Double

data ContDist = Beta      Alpha Beta
              | Gaussian  Mu    Sigma
              | Uniform     Double Double
              | Exponential Double
              | Poisson     Double

data Expr = Add   Expr Expr ContDist
        | Sub   Expr Expr ContDist
        | Mul   Expr Expr ContDist
        | Div   Expr Expr ContDist
        | Mod   Expr Expr ContDist -- modulo operator. 
        | Lit   Int ContDist
        | Var   Var -- eerder gedeclareerde variablen kunnen hier gebruikt worden.

evalStatement :: Statement -> S.State Env Int 
evalStatement (Stat x (Var y))   = do 
    env <- S.get  
    -- verander de env, 
    -- de distributie van x wordt verandert naar die van y. (overwrite)

    let distInt = env M.! x
    return 0