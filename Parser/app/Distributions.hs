{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Distributions (contDist, dscrDist) where

-- import qualified Main as M
import qualified Data.Ratio as DR
import qualified Data.Map as M

type Prob = Rational
newtype Dist a = D { runD :: M.Map a Prob }


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
              
data DscrDist = Bernoulli   Double

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


