module Main (main) where
import qualified Test.QuickCheck as Q
import qualified Test.HUnit as H

import Tests.DistributionTest
import Tests.EvaluatorTest
import Tests.ParserTest

main :: IO ()
main = do

-- ----------------------------------------
-- ParserTest
-- ----------------------------------------

  putStrLn "test_comp_parser"
  _ <- H.runTestTT test_comp_parser
  putStrLn "\n"

  putStrLn "test_cond_parser"
  _ <- H.runTestTT test_cond_parser
  putStrLn "\n"

  putStrLn "test_stat_parser1"
  _ <- H.runTestTT test_stat_parser1
  putStrLn "\n"

  putStrLn "test_stat_parser2"
  _ <- H.runTestTT test_stat_parser2
  putStrLn "\n"

  putStrLn "test_stat_parser3"
  _ <- H.runTestTT test_stat_parser3
  putStrLn "\n"

  putStrLn "test_var_parser"
  _ <- H.runTestTT test_var_parser
  putStrLn "\n"

-- ----------------------------------------
-- EvluatorTest
-- ----------------------------------------

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

-- ----------------------------------------
-- DistributionsTest
-- ----------------------------------------

  putStrLn "prop_sum_to_one_contDist:"
  _ <- Q.quickCheck prop_sum_to_one_contDist
  putStrLn "\n"

  putStrLn "prop_sum_to_one_dscrDist:"
  _ <- Q.quickCheck prop_sum_to_one_dscrDist
  putStrLn "\n"

  return ()