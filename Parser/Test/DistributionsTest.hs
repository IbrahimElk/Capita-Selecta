
module Test.DistributionTest where

-- commands: 
-- stack ghci --package QuickCheck test/DistributionsTest.hs 
-- ghci Test/DistributionsTest.hs  -package QuickCheck 

-- uncomment this when testing :
import Test.QuickCheck
import Src.Representation (Kuifje(..), Statement(..), Expr(..), Cond(..), Comp(..), Variable,
    ContDist(..), DscrDist(..), skip, returns, update, while, cond,
    Dist(..), Prob(..), Env(..) )
import Src.Distributions (addDist, subDist, mulDist, divDist, 
    litDist, boolDist, contDist, dscrDist)

import qualified Data.Map as M

-- Property 1: Distribution Sum Property
prop_sum_probability :: (Eq a, Num a) => Dist a -> Bool
prop_sum_probability (D dist) = sum (M.elems dist) == 1

-- -- Property 2: Distribution Range Property
-- prop_range_coverage :: (Enum a, Ord a) => Dist a -> Bool
-- prop_range_coverage (D dist) = M.keysSet dist == M.keysSet (M.fromList [(minBound, 0), (maxBound, 0)])

-- -- Property 3: Operation Distribution Property
-- prop_operation_distribution :: (Eq a, Num a) => (ContDist -> Dist a) -> ContDist -> a -> a -> Bool
-- prop_operation_distribution opDistFunc opDist x y =
--     let resultDist = opDistFunc opDist
--         resultValues = M.keysSet (runD resultDist)
--         xDist = runD $ contDist undefined
--         yDist = runD $ contDist undefined
--         expectedDist = runD $ contDist undefined
--         expectedValues = M.keysSet expectedDist
--     in M.keysSet xDist `isSubsetOf` resultValues && M.keysSet yDist `isSubsetOf` resultValues && resultValues == expectedValues

-- -- Property 4: Literal Distribution Property
-- prop_literal_distribution :: Bool -> Bool
-- prop_literal_distribution b = b `elem` M.keys (runD litDist)

-- -- Property 5: Boolean Distribution Property
-- prop_boolean_distribution :: Bool -> Bool
-- prop_boolean_distribution b = b `elem` M.keys (runD boolDist)

-- Run QuickCheck tests
main :: IO ()
main = do
    putStrLn "Checking Distribution Properties..."
    -- quickCheck (prop_sum_probability :: Dist Int -> Bool)
    -- quickCheck (prop_range_coverage :: Dist Int -> Bool)
    -- putStrLn "Checking Operation Distribution Properties..."
    -- quickCheck (prop_operation_distribution addDist)
    -- quickCheck (prop_operation_distribution subDist)
    -- quickCheck (prop_operation_distribution mulDist)
    -- quickCheck (prop_operation_distribution divDist)
    -- putStrLn "Checking Literal Distribution Property..."
    -- quickCheck prop_literal_distribution
    -- putStrLn "Checking Boolean Distribution Property..."
    -- quickCheck prop_boolean_distribution
