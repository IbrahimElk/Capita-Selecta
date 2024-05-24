{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Parser.Src.Distributions (addDist, subDist, mulDist, divDist, litDist, boolDist, contDist, dscrDist) where

-- import qualified Main as M
import qualified Data.Ratio as DR
import qualified Data.Map as M
import qualified Parser.Src.Representation as Rp
import Parser.Src.Representation (Dist(..),Env )

-- ------------------------------------
-- random function : 
-- ------------------------------------

contDist :: Rp.ContDist -> Dist Int
contDist _ =  hardcodedContDist

hardcodedContDist :: Dist Int
hardcodedContDist = D $ M.fromList
    [(fromIntegral x, p) | x <- [-1, 0, 1], let p = if x == 0 then 1/2 else 1/4]

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
