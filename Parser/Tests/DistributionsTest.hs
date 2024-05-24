
module Parser.Tests.DistributionTest where
import Test.QuickCheck
import qualified Data.Map as M

import Parser.Src.Representation (Kuifje(..), Statement(..), Expr(..), Cond(..), Comp(..), Variable, ContDist(..), DscrDist(..), skip, returns, update, while, cond, Dist(..), Prob(..), Env(..) )
import Parser.Src.Distributions (contDist, dscrDist)

-- *Parser.Tests.DistributionTest> quickCheck prop_sum_to_one_contDist 
-- +++ OK, passed 100 tests.
prop_sum_to_one_contDist :: Property
prop_sum_to_one_contDist = 
  let D distMap = contDist undefined
      sumProbs = sum $ M.elems distMap
  in
    counterexample ("Sum of probabilities is not 1: " ++ show sumProbs) $
      abs (sumProbs - 1) <= 1e-6

-- *Parser.Tests.DistributionTest> quickCheck prop_sum_to_one_dscrDist 
-- +++ OK, passed 100 tests.
prop_sum_to_one_dscrDist :: Property
prop_sum_to_one_dscrDist = 
  let D distMap = dscrDist undefined
      sumProbs = sum $ M.elems distMap
  in
    counterexample ("Sum of probabilities is not 1: " ++ show sumProbs) $
      abs (sumProbs - 1) <= 1e-6