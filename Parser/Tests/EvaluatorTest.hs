


module Tests.EvaluatorTest 
  (module Tests.EvaluatorTest)
  where
import qualified Test.QuickCheck as Q
import qualified Test.HUnit as H
import qualified Data.Map as M

import Src.Representation
import qualified Src.Evaluator as E

prop_probability_point_is_one :: Int -> Q.Property
prop_probability_point_is_one x =
  Q.counterexample ("Probability of " ++ show x ++ " is not 1") $
    let D distMap = E.point x
        p = distMap M.! x
    in p == 1

-- ---------------------------------------------------
--  tests for arithmatic distribution operators 
-- ---------------------------------------------------


prop_symmetric_sumDistributions :: M.Map Int Prob -> M.Map Int Prob -> Q.Property
prop_symmetric_sumDistributions dist1 dist2 =
  M.size dist1 >= 2 && M.size dist2 >= 2 Q.==>
  let result1 = runD $ E.sumDistributions (D dist1) (D dist2)
      result2 = runD $ E.sumDistributions (D dist2) (D dist1)
  in Q.counterexample "Results are not symmetric" $ result1 == result2

prop_symmetric_mulDistributions :: M.Map Int Prob -> M.Map Int Prob -> Q.Property
prop_symmetric_mulDistributions dist1 dist2 =
  M.size dist1 >= 2 && M.size dist2 >= 2 Q.==>  
  let result1 = runD $ E.mulDistributions (D dist1) (D dist2)
      result2 = runD $ E.mulDistributions (D dist2) (D dist1)
  in Q.counterexample "Results are not symmetric" $ result1 == result2

prop_normalizeBDistribution :: M.Map Bool Prob -> Q.Property
prop_normalizeBDistribution dist =
  (M.size dist == 2) Q.==>
  let distWithoutNegativeProbs = M.map (\p -> if p < 0 then -p else p) dist -- due to generation from arbirary Rational. 
      normalizedDist = runD $ E.normalizeBDistribution (D distWithoutNegativeProbs)
      totalProb = sum (M.elems normalizedDist)
  in Q.counterexample ("Total probability: " ++ show totalProb) $ totalProb == 1

prop_normalizeDistribution :: M.Map Int Prob -> Q.Property
prop_normalizeDistribution dist =
  M.size dist >= 2 Q.==>
  let normalizedDist = runD $ E.normalizeDistribution (D dist)
      totalProb = sum (M.elems normalizedDist)
      amount_zeros = M.size $ M.filter (== 0) normalizedDist
  in Q.counterexample ("Total probability: " ++ show totalProb) $ totalProb == 1 && amount_zeros == 0

test_sumDistributions :: H.Test
test_sumDistributions = H.TestList
  [H.TestCase $ H.assertEqual "Test case 1" expected1 actual1]
  where
    dist1 = M.fromList [(1, 1/2), (2, 1/3), (3, 1/6)]
    dist2 = M.fromList [(1, 1/4), (2, 1/4), (3, 1/2)]

    -- 1 + 1, 1/2 * 1/4 => (2, 1/8)
    -- 1 + 2, 1/2 * 1/4 and 2 + 1, 1/3 * 1/4 => (3, 1/2 * 1/4 +  1/3 * 1/4) =  (3, 5/24)
    expected1 = M.fromList [(2, 1/8), (3, 5/24), (4, 9/24), (5, 5/24), (6, 2/24)]
    D actual1 = E.sumDistributions (D dist1) (D dist2)

test_subDistributions :: H.Test
test_subDistributions = H.TestList
  [H.TestCase $ H.assertEqual "Test case 2" expected1 actual1]
  where
    dist1 = M.fromList [(1, 1/2), (2, 1/3), (3, 1/6)]
    dist2 = M.fromList [(1, 1/4), (2, 1/4), (3, 1/2)]

    expected1 = M.fromList [(-2, 1/4), (-1, 7/24), (0, 7/24), (1, 1/8), (2, 1/24)]
    D actual1 = E.subDistributions (D dist1) (D dist2)

test_mulDistributions :: H.Test
test_mulDistributions = H.TestList
  [H.TestCase $ H.assertEqual "Test case 3" expected1 actual1]
  where
    dist1 = M.fromList [(1, 1/2), (2, 1/3), (3, 1/6)]
    dist2 = M.fromList [(1, 1/4), (2, 1/4), (3, 1/2)]

    expected1 = M.fromList [(-2, 1/4), (-1, 7/24), (0, 7/24), (1, 1/8), (2, 1/24)]
    D actual1 = E.mulDistributions (D dist1) (D dist2)

test_divDistributions :: H.Test
test_divDistributions = H.TestList
  [H.TestCase $ H.assertEqual "Test case 4" expected1 actual1]
  where
    dist1 = M.fromList [(1, 1/2), (2, 1/3), (3, 1/6)]
    dist2 = M.fromList [(1, 1/4), (2, 1/4), (3, 1/2)]

    expected1 = M.fromList [(-2, 1/4), (-1, 7/24), (0, 7/24), (1, 1/8), (2, 1/24)]
    D actual1 = E.divDistributions (D dist1) (D dist2)

test_modDistributions :: H.Test
test_modDistributions = H.TestList
  [H.TestCase $ H.assertEqual "Test case 4" expected1 actual1]
  where
    dist1 = M.fromList [(1, 1/2), (2, 1/3), (3, 1/6)]
    dist2 = M.fromList [(1, 1/4), (2, 1/4), (3, 1/2)]

    expected1 = M.fromList [(-2, 1/4), (-1, 7/24), (0, 7/24), (1, 1/8), (2, 1/24)]
    D actual1 = E.modDistributions (D dist1) (D dist2)

-- ---------------------------------------------------
-- TODO: tests for boolean distribution operators 
-- ---------------------------------------------------


-- ---------------------------------------------------
-- TODO: tests for EvalExpr, evalStatement, evalComp, evalCond
-- ---------------------------------------------------


-- ---------------------------------------------------
-- TODO: tests for evaluate
-- ---------------------------------------------------


mainE :: IO ()
mainE = do
  putStrLn "prop_symmetric_sumDistributions:"
  _ <- Q.quickCheck prop_symmetric_sumDistributions
  putStrLn "\n"

  putStrLn "prop_symmetric_mulDistributions:"
  _ <- Q.quickCheck prop_symmetric_mulDistributions
  putStrLn "\n"

  putStrLn "prop_normalizeBDistribution:"
  _ <- Q.quickCheck prop_normalizeBDistribution
  putStrLn "\n"
  
  putStrLn "prop_normalizeDistribution:"
  _ <- Q.quickCheck prop_normalizeDistribution
  putStrLn "\n"

  putStrLn "test_sumDistributions"
  _ <- H.runTestTT test_sumDistributions
  putStrLn "\n"

  putStrLn "test_subDistributions"
  _ <- H.runTestTT test_subDistributions
  putStrLn "\n"

  return ()


