module Src.Distributions (
  module Src.Distributions
) where

import qualified Data.Ratio as DR
import qualified Data.Map as M
import Src.Representation
-- ------------------------------------
-- random function : 
-- ------------------------------------

contDist :: ContDist -> Dist Int
contDist _ =  hardcodedContDist

hardcodedContDist :: Dist Int
hardcodedContDist = D $ M.fromList
    [(fromIntegral x, p) | x <- [-1, 0, 1], let p = if x == 0 then 1/2 else 1/4]

dscrDist :: DscrDist -> Dist Bool
dscrDist _ =  hardcodedDscrDist

hardcodedDscrDist ::Dist Bool
hardcodedDscrDist = D $ M.fromList [(True, 1 DR.% 2), (False, 1 DR.% 2)]


-- per operation dus Add, SUb , store their noise distrobutions.
addDist :: ContDist
addDist = Gaussian 0 1

subDist :: ContDist
subDist = Gaussian 0 1

divDist :: ContDist
divDist = Gaussian 0 1

litDist :: ContDist
litDist = Gaussian 0 1

mulDist :: ContDist
mulDist = Gaussian 0 1

boolDist :: DscrDist
boolDist = Bernoulli 0
