-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE TypeOperators #-}

-- import qualified Data.Map as M
-- import qualified Text.Parsec as P
-- import qualified Text.Parsec.String as PS
-- import Data.Map.Strict ( singleton , mapWithKey)

-- type Prob = Rational
-- newtype Dist a = D { runD :: M.Map a Prob }

-- type a ~> b = a -> Dist b

-- data Kuifje s
--   = Skip
--   | Update (s ~> s) (Kuifje s)
--   | If (s ~> Bool) (Kuifje s) (Kuifje s)
--   | While (s ~> Bool) (Kuifje s)
--   | forall o. (Ord o, Show o) => Observe (s ~> o) (Kuifje s)

-- instance Semigroup (Kuifje s) where
--   Skip        <> k = k
--   Update f p  <> k = Update f (p <> k)
--   While c p   <> k = While c (p <> k)
--   If c p q    <> k = If c p (q <> k)
--   Observe f p <> k = Observe f (p <> k)

-- instance Monoid (Kuifje s) where
--   mempty = Skip
--   mappend = (<>)

-- skip :: Kuifje s
-- skip = Skip

-- update' :: (s ~> s) -> Kuifje s
-- update' f = Update f skip

-- while :: (s ~> Bool) -> Kuifje s -> Kuifje s
-- while = While

-- cond :: (s ~> Bool) -> Kuifje s -> Kuifje s -> Kuifje s
-- cond = If

-- observe :: (Ord o, Show o) => (s ~> o) -> Kuifje s
-- observe o = Observe o skip

-- voorbeeldString :: String
-- voorbeeldString = "Update statement1 (While statement2 (Update statement3 (Update statement4 (Skip))))"

-- kuifjeParser :: PS.Parser (Kuifje Int)
-- kuifjeParser = updateParser P.<|> whileParser P.<|> skipParser P.<|> condParser -- <|> observeParser

-- skipParser :: PS.Parser (Kuifje Int)
-- skipParser = do
--     P.string "Skip"
--     return Skip

-- updateParser :: PS.Parser (Kuifje Int)
-- updateParser = do
--     P.string "Update"
--     P.spaces
--     -- dit werkt met <- , zet om van "PS.Parser (Kuifje Int)" type naar Kuifje int type. 
--     statement <- statementParsers
--     nestedKuifje <- nestedKuifjeParser
--     return (Update statement nestedKuifje)

-- whileParser :: PS.Parser (Kuifje Int)
-- whileParser = do
--     P.string "While"
--     P.spaces
--     condition <- boolStatementParsers
--     nestedKuifje <- nestedKuifjeParser
--     return (While condition nestedKuifje)

-- condParser :: PS.Parser (Kuifje Int)
-- condParser = do
--     P.string "If"
--     P.spaces
--     condition <- boolStatementParsers
--     nestedKuifjeIf <- nestedKuifjeParser
--     nestedKuifjeElse <- nestedKuifjeParser
--     return (If condition nestedKuifjeIf nestedKuifjeElse)

-- -- ------------------------
-- -- Helper functies.
-- -- ------------------------

-- statementParsers :: PS.Parser (Int ~> Int)
-- statementParsers = head (map tryThisStatement ["statement1", "statement3", "statement4"])

-- tryThisStatement :: String -> PS.Parser (Int ~> Int)
-- tryThisStatement s = do
--     P.try (
--         P.string s >>
--         return (strToFunction s))

-- strToFunction :: String -> (Int ~> Int)
-- strToFunction "statement1" = statement1
-- strToFunction "statement3" = statement3
-- strToFunction "statement4" = statement4

-- nestedKuifjeParser :: PS.Parser (Kuifje Int)
-- nestedKuifjeParser = do
--   P.char '('
--   P.spaces
--   p <- kuifjeParser
--   P.spaces
--   P.char ')'
--   return p

-- boolStatementParsers :: PS.Parser (Int ~> Bool)
-- boolStatementParsers = head (map tryThisCondition ["statement2"])

-- tryThisCondition :: String -> PS.Parser (Int ~> Bool)
-- tryThisCondition s = do
--     P.try (
--         P.string s >>
--         return (strToBoolFunction s))

-- strToBoolFunction :: String -> (Int ~> Bool)
-- strToBoolFunction "statement2" = statement2


-- -- ----------------- 
-- -- Util
-- -- -----------------

-- point :: (Ord a) => a -> Dist a
-- point x = D $ singleton x 1

-- -- later nog complexere distributies toevoegen. 
-- statement1 :: Int -> Dist Int
-- statement1 x = point (x + 1)

-- statement2 :: Int -> Dist Bool
-- statement2 x = point (x < 0)

-- statement3 :: Int -> Dist Int
-- statement3 x = point (x - 2)

-- statement4 :: Int -> Dist Int
-- statement4 x = point (x + 1)


-- -- -------------------
-- -- evaluator
-- -- -------------------
-- evaluate :: Int -> Kuifje Int -> Int
-- evaluate x Skip = x
-- evaluate state (Update f next) = do
--   let newDist = f state
--       newState = sampleInt newDist
--   evaluate newState next

-- evaluate state (If condStmt trueBranch falseBranch) = do
--   let condDist = condStmt state
--       condResult = sampleBool condDist
--   if condResult
--     then evaluate state trueBranch
--     else evaluate state falseBranch

-- evaluate state (While condStmt body) = do
--   let condDist = condStmt state
--       condResult = sampleBool condDist
--   if condResult
--     then do
--       let
--         newState = evaluate state body
--       evaluate newState (While condStmt body)
--     else
--       state

-- evaluate state (Observe f e) = error "TBD"


-- average :: Dist Int -> Rational
-- average = sum . mapWithKey multiplyIntRational . runD

-- sampleInt :: Dist Int -> Int
-- sampleInt =  rationalToInteger . average

-- sampleBool :: Dist Bool -> Bool
-- sampleBool = intToBool . sampleInt . boolDistToIntDist

-- -- -------
-- -- Utils
-- -- -------

-- multiplyIntRational :: Int -> Rational -> Rational
-- multiplyIntRational int rational = toRational int * rational

-- rationalToInteger :: Rational -> Int
-- rationalToInteger = round

-- boolToInt :: Bool -> Int
-- boolToInt True  = 1
-- boolToInt False = 0

-- intToBool :: Int -> Bool
-- intToBool 1 = True
-- intToBool 0 = False
-- intToBool _ = error "nice"

-- boolDistToIntDist :: Dist Bool -> Dist Int
-- boolDistToIntDist (D boolDist) = D $ M.mapKeysMonotonic boolToInt boolDist


-- -- --------------------
-- -- 
-- -- --------------------

-- -- volledig stostisch calculator bouwen
-- -- en vragen aan prof hoe sample te nemen 
-- -- en hoe code in verband met statement beter scrhijven. 
-- -- mss evaluator ook met monads eventueel idk.

-- -- calculator :: Int -> Operator -> Int
-- -- calculator = error "nah"

-- -- basically statement1 tot 4 vervangen met actually calculator logic. 


-- -- "Update (+ 5) (Update (- 5) (Skip))" 
-- -- "Update (* 5) (Update (/ 5) (If isPrime (Skip) (Skip)))" 


-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE TypeOperators #-}

-- import qualified Data.Map as M
-- import qualified Text.Parsec as P
-- import qualified Text.Parsec.String as PS
-- import Data.Map.Strict ( singleton , mapWithKey)

-- type Prob = Rational
-- newtype Dist a = D { runD :: M.Map a Prob }

-- type a ~> b = a -> Dist b

-- data Kuifje s
--   = Skip
--   | Update (s ~> s) (Kuifje s)
--   | If (s ~> Bool) (Kuifje s) (Kuifje s)
--   | While (s ~> Bool) (Kuifje s)
--   | forall o. (Ord o, Show o) => Observe (s ~> o) (Kuifje s)

-- instance Semigroup (Kuifje s) where
--   Skip        <> k = k
--   Update f p  <> k = Update f (p <> k)
--   While c p   <> k = While c (p <> k)
--   If c p q    <> k = If c p (q <> k)
--   Observe f p <> k = Observe f (p <> k)

-- instance Monoid (Kuifje s) where
--   mempty = Skip
--   mappend = (<>)

-- skip :: Kuifje s
-- skip = Skip

-- update' :: (s ~> s) -> Kuifje s
-- update' f = Update f skip

-- while :: (s ~> Bool) -> Kuifje s -> Kuifje s
-- while = While

-- cond :: (s ~> Bool) -> Kuifje s -> Kuifje s -> Kuifje s
-- cond = If

-- observe :: (Ord o, Show o) => (s ~> o) -> Kuifje s
-- observe o = Observe o skip

-- voorbeeldString :: String
-- voorbeeldString = "Update statement1 (While statement2 (Update statement3 (Update statement4 (Skip))))"

-- kuifjeParser :: PS.Parser (Kuifje Int)
-- kuifjeParser = updateParser P.<|> whileParser P.<|> skipParser P.<|> condParser -- <|> observeParser

-- skipParser :: PS.Parser (Kuifje Int)
-- skipParser = do
--     P.string "Skip"
--     return Skip

-- updateParser :: PS.Parser (Kuifje Int)
-- updateParser = do
--     P.string "Update"
--     P.spaces
--     -- dit werkt met <- , zet om van "PS.Parser (Kuifje Int)" type naar Kuifje int type. 
--     statement <- statementParsers
--     nestedKuifje <- nestedKuifjeParser
--     return (Update statement nestedKuifje)

-- whileParser :: PS.Parser (Kuifje Int)
-- whileParser = do
--     P.string "While"
--     P.spaces
--     condition <- boolStatementParsers
--     nestedKuifje <- nestedKuifjeParser
--     return (While condition nestedKuifje)

-- condParser :: PS.Parser (Kuifje Int)
-- condParser = do
--     P.string "If"
--     P.spaces
--     condition <- boolStatementParsers
--     nestedKuifjeIf <- nestedKuifjeParser
--     nestedKuifjeElse <- nestedKuifjeParser
--     return (If condition nestedKuifjeIf nestedKuifjeElse)

-- -- ------------------------
-- -- Helper functies.
-- -- ------------------------

-- statementParsers :: PS.Parser (Int ~> Int)
-- statementParsers = head (map tryThisStatement ["statement1", "statement3", "statement4"])

-- tryThisStatement :: String -> PS.Parser (Int ~> Int)
-- tryThisStatement s = do
--     P.try (
--         P.string s >>
--         return (strToFunction s))

-- strToFunction :: String -> (Int ~> Int)
-- strToFunction "statement1" = statement1
-- strToFunction "statement3" = statement3
-- strToFunction "statement4" = statement4

-- nestedKuifjeParser :: PS.Parser (Kuifje Int)
-- nestedKuifjeParser = do
--   P.char '('
--   P.spaces
--   p <- kuifjeParser
--   P.spaces
--   P.char ')'
--   return p

-- boolStatementParsers :: PS.Parser (Int ~> Bool)
-- boolStatementParsers = head (map tryThisCondition ["statement2"])

-- tryThisCondition :: String -> PS.Parser (Int ~> Bool)
-- tryThisCondition s = do
--     P.try (
--         P.string s >>
--         return (strToBoolFunction s))

-- strToBoolFunction :: String -> (Int ~> Bool)
-- strToBoolFunction "statement2" = statement2


-- -- ----------------- 
-- -- Util
-- -- -----------------

-- point :: (Ord a) => a -> Dist a
-- point x = D $ singleton x 1

-- -- later nog complexere distributies toevoegen. 
-- statement1 :: Int -> Dist Int
-- statement1 x = point (x + 1)

-- statement2 :: Int -> Dist Bool
-- statement2 x = point (x < 0)

-- statement3 :: Int -> Dist Int
-- statement3 x = point (x - 2)

-- statement4 :: Int -> Dist Int
-- statement4 x = point (x + 1)


-- -- -------------------
-- -- evaluator
-- -- -------------------
-- evaluate :: Int -> Kuifje Int -> Int
-- evaluate x Skip = x
-- evaluate state (Update f next) = do
--   let newDist = f state
--       newState = sampleInt newDist
--   evaluate newState next

-- evaluate state (If condStmt trueBranch falseBranch) = do
--   let condDist = condStmt state
--       condResult = sampleBool condDist
--   if condResult
--     then evaluate state trueBranch
--     else evaluate state falseBranch

-- evaluate state (While condStmt body) = do
--   let condDist = condStmt state
--       condResult = sampleBool condDist
--   if condResult
--     then do
--       let
--         newState = evaluate state body
--       evaluate newState (While condStmt body)
--     else
--       state

-- evaluate state (Observe f e) = error "TBD"


-- average :: Dist Int -> Rational
-- average = sum . mapWithKey multiplyIntRational . runD

-- sampleInt :: Dist Int -> Int
-- sampleInt =  rationalToInteger . average

-- sampleBool :: Dist Bool -> Bool
-- sampleBool = intToBool . sampleInt . boolDistToIntDist

-- -- -------
-- -- Utils
-- -- -------

-- multiplyIntRational :: Int -> Rational -> Rational
-- multiplyIntRational int rational = toRational int * rational

-- rationalToInteger :: Rational -> Int
-- rationalToInteger = round

-- boolToInt :: Bool -> Int
-- boolToInt True  = 1
-- boolToInt False = 0

-- intToBool :: Int -> Bool
-- intToBool 1 = True
-- intToBool 0 = False
-- intToBool _ = error "nice"

-- boolDistToIntDist :: Dist Bool -> Dist Int
-- boolDistToIntDist (D boolDist) = D $ M.mapKeysMonotonic boolToInt boolDist


-- -- --------------------
-- -- 
-- -- --------------------

-- -- volledig stostisch calculator bouwen
-- -- en vragen aan prof hoe sample te nemen 
-- -- en hoe code in verband met statement beter scrhijven. 
-- -- mss evaluator ook met monads eventueel idk.

-- -- calculator :: Int -> Operator -> Int
-- -- calculator = error "nah"

-- -- basically statement1 tot 4 vervangen met actually calculator logic. 


-- -- "Update (+ 5) (Update (- 5) (Skip))" 
-- -- "Update (* 5) (Update (/ 5) (If isPrime (Skip) (Skip)))" 





