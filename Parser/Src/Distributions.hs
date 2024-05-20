{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Src.Distributions (addDist, subDist, mulDist, divDist, litDist, boolDist, contDist, dscrDist) where

-- import qualified Main as M
import qualified Data.Ratio as DR
import qualified Data.Map as M
import qualified Src.Representation as Rp
import Src.Representation (Dist(..),Env ) 

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

hardcodedDscrDist ::Dist Bool
hardcodedDscrDist = D $ M.fromList [(True, 1 DR.% 2), (False, 1 DR.% 2)]


-- per operation dus Add, SUb , store their noise distrobutions.
addDist :: Rp.ContDist 
addDist = Rp.Gaussian 0 1

subDist :: Rp.ContDist 
subDist = Rp.Gaussian 0 1

divDist :: Rp.ContDist 
divDist = Rp.Gaussian 0 1

litDist :: Rp.ContDist 
litDist = Rp.Gaussian 0 1

mulDist :: Rp.ContDist 
mulDist = Rp.Gaussian 0 1

boolDist :: Rp.DscrDist 
boolDist = Rp.Bernoulli 0
