module Tests.ParserTest 
  (module Tests.ParserTest)
  where
    
import qualified Test.HUnit as H
import qualified Text.Parsec as P

import Src.Representation
import qualified Src.Distributions as D
import qualified Src.Parserator as Ps

statement1 :: Statement
statement1 = Stat "x" (Add (Lit 2 D.litDist) (Lit 1 D.litDist) D.addDist)

test_stat_parser1 :: H.Test
test_stat_parser1 = H.TestList
  [H.TestCase $ do
      let parsedStatement = P.parse Ps.parseStatement "" (show statement1)
      case parsedStatement of
        Left err -> H.assertFailure ("Parse error: " ++ show err)
        Right actual1 -> H.assertEqual "Test case 1" statement1 actual1
  ]

statement20 :: Comp
statement20 = Equal (Var "x") (Lit 3 D.litDist) D.boolDist

statementstr20 :: String
statementstr20 = "x == 3"

test_cond_parser :: H.Test
test_cond_parser = H.TestList
  [H.TestCase $ do
      let parsedCond = P.parse Ps.parseEqual "" statementstr20
      case parsedCond of
        Left err -> H.assertFailure ("Parse error: " ++ show err)
        Right actual1 -> H.assertEqual "Test case 2" statement20 actual1
  ]

statement21 :: Cond
statement21 = Comp (NotEqual (Var "x") (Lit 3 D.litDist) D.boolDist)

statementstr21 :: String
statementstr21 = "x != 3"

test_comp_parser :: H.Test
test_comp_parser = H.TestList
  [H.TestCase $ do
      let parsedComp = P.parse Ps.parseCond "" statementstr21
      case parsedComp of
        Left err -> H.assertFailure ("Parse error: " ++ show err)
        Right actual1 -> H.assertEqual "Test case 3" statement21 actual1
  ]

statement3 :: Statement
statement3 = Stat   "x" (Sub (Lit 3 D.litDist) (Lit 1 D.litDist) D.addDist)

test_stat_parser2 :: H.Test
test_stat_parser2 = H.TestList
  [H.TestCase $ do
      let parsedStat = P.parse Ps.parseStatement "" (show statement3)
      case parsedStat of
        Left err -> H.assertFailure ("Parse error: " ++ show err)
        Right actual1 -> H.assertEqual "Test case 4" statement3 actual1
  ]

statement4 :: Statement
statement4 = Stat   "t" (Mul (Var "x") (Lit 2 D.litDist) D.addDist)

test_stat_parser3 :: H.Test
test_stat_parser3 = H.TestList
  [H.TestCase $ do
      let parsedStat = P.parse Ps.parseStatement "" (show statement4)
      case parsedStat of
        Left err -> H.assertFailure ("Parse error: " ++ show err)
        Right actual1 -> H.assertEqual "Test case 5" statement4 actual1
  ]

statement5 :: Variable
statement5 = "x"

test_var_parser :: H.Test
test_var_parser = H.TestList
  [H.TestCase $ do
      let parsedVar = P.parse Ps.parseVar "" statement5
      case parsedVar of
        Left err -> H.assertFailure ("Parse error: " ++ show err)
        Right actual1 -> H.assertEqual "Test case 6" statement5 actual1
  ]


mainP :: IO ()
mainP = do

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


-- program1 :: R.Kuifje
-- program1
--   = R.update statement1 <>  
--     R.while statement20 (R.update statement3) (R.update statement4) <> 
--     R.update statement1 <>
--     R.cond statement21 (R.update statement3) (R.update statement4) (R.update statement1) <>
--     R.returns statement5

-- program2 :: R.Kuifje
-- program2
--   = R.update statement1


-- instance Arbitrary Statement where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then Stat <$> arbitrary <*> arbitrary
--       else Stat <$> arbitrary <*> resize (n - 1) arbitrary

-- instance Arbitrary ContDist where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then oneof
--         [ Beta      <$> arbitrary <*> arbitrary
--         , Gaussian  <$> arbitrary <*> arbitrary
--         , Uniform   <$> arbitrary <*> arbitrary
--         , Exponential <$> arbitrary
--         , Poisson   <$> arbitrary
--         ]
--       else oneof
--         [ Beta      <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Gaussian  <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Uniform   <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Exponential <$> resize (n - 1) arbitrary
--         , Poisson   <$> resize (n - 1) arbitrary
--         ]

-- instance Arbitrary DscrDist where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then Bernoulli <$> arbitrary
--       else Bernoulli <$> resize (n - 1) arbitrary

-- -- Instance for Expr
-- instance Arbitrary Expr where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then oneof
--         [ Lit <$> arbitrary <*> arbitrary
--         , Var <$> arbitrary
--         ]
--       else oneof
--         [ Add <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Sub <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Mul <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Div <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Mod <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Lit <$> arbitrary <*> resize (n - 1) arbitrary
--         , Var <$> resize (n - 1) arbitrary
--         ]

-- instance Arbitrary Comp where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then oneof
--         [ Equal               <$> arbitrary <*> arbitrary <*> arbitrary
--         , NotEqual            <$> arbitrary <*> arbitrary <*> arbitrary
--         , LessThan            <$> arbitrary <*> arbitrary <*> arbitrary
--         , GreaterThan         <$> arbitrary <*> arbitrary <*> arbitrary
--         , LessThanOrEqual     <$> arbitrary <*> arbitrary <*> arbitrary
--         , GreaterThanOrEqual  <$> arbitrary <*> arbitrary <*> arbitrary
--         ]
--       else oneof
--         [ Equal               <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , NotEqual            <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , LessThan            <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , GreaterThan         <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , LessThanOrEqual     <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , GreaterThanOrEqual  <$> arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         ]

-- instance Arbitrary Cond where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then oneof
--         [ Comp <$> arbitrary
--         , And  <$> arbitrary <*> arbitrary <*> arbitrary
--         , Or   <$> arbitrary <*> arbitrary <*> arbitrary
--         ]
--       else oneof
--         [ Comp <$> resize (n - 1) arbitrary
--         , And  <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , Or   <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         ]

-- instance Arbitrary Kuifje where
--   arbitrary = sized $ \n ->
--     if n == 0
--       then oneof
--         [ return Skip
--         , Return <$> arbitrary
--         ]
--       else oneof
--         [ return Skip
--         , Return <$> resize (n - 1) arbitrary
--         , Update <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , If <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         , While <$> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary <*> resize (n - 1) arbitrary
--         ]

