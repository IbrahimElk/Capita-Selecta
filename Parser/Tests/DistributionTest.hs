
module Tests.DistributionTest 
  (module Tests.DistributionTest)
  where
import qualified Data.Map as M
import Test.QuickCheck

import Src.Representation (Dist(..))
import Src.Distributions (contDist, dscrDist)

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


mainD :: IO ()
mainD = do
  putStrLn "prop_sum_to_one_contDist:"
  _ <- quickCheck prop_sum_to_one_contDist
  putStrLn "\n"

  putStrLn "prop_sum_to_one_dscrDist:"
  _ <- quickCheck prop_sum_to_one_dscrDist
  putStrLn "\n"
  return ()


