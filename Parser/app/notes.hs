
-- van prof: 
-- deeply embed the expression language:
-- dus deeply embed the expression => vorm (s ~> s) om naar een datetype abstract syntax tree
-- bv. Var Expr 
-- met Expr kun je dan heel andere structuren kijrgen zols e1 <p> e2 , dus e1 met kans p applied en e2 met kans 1-p

-- turn into stand-alone DSL
-- dat je dus een programma kunt scrhijven in kuifje taal zonder haskell, 
-- en om die te runnen dus parsed. dit is mogelijk want je hebt de functie s~>s minder algemeen gemaakt, door dus die deeply mebedded expression langyuage. 

-- data Expr = -- Expr <p> Expr | 


-- voorbeelden van kuifje programmas vanuit de paper: 
-- 1) 
  -- y := 0;
  -- while (x > 0) {
  -- y := y + x;
  -- x := x - 1;
  -- }

-- Note that notation like s.y := (s.y + s.x ), with the obvious meaning, is not pseudo-code, but
-- more familiar rendering of valid Haskell code based on lens library17 operators. We refer the
-- interested reader to this paper’s companion code for the details.
-- https://hackage.haskell.org/package/lens
-- aan elke operatie specifieer je een probabiliteit. 
-- een parameter bv. gaussian distribution parameters; 
-- mu en sigma
-- en wanneer we parsen,
-- dan perturberen we het resultaat van de operatie 
-- met die gegeven parameters en distributie. 
-- elk operatie kan eigen parameters hebben
-- want sommige operaties zijn nu eenmaal numeriek stabieler dan andere. 
-- hetzelfd met Lit , want een reel getal, kun je niet precies defineren in 32 , 64 of 128 bits. 

-- -----------
-- -----------
-- idees voor expr datatype:
-- -----------
-- -----------

-- Of ge voegt deze volgene extensies toe, 
-- om alle mogelijke mathematische programma's te kunnen schrijven 
-- met de kuifje programeertaal.  

-- of ge implementeert functies, 
-- zodat mensen fincties kunnen schrijven die dus verschillende 
-- onderlinge kuifje programma's bevatten zodat ze eigen logartime functie bv. 
-- kunnen implementerne. 

-- data Func = Function Kuifje  
-- data KuifjeProgramma = Prog [Func]

-- geopteerd voor eerste keuze, want makkelijker. 
-- kan later gedaan worden als tijd over. 

-- extensions: 
-- | Pow   Double Expr   -- Given a real number b such that: x^b
-- | Log   Double Expr   -- Given a positive real number b such that b ≠ 1: log_b(x)
-- | Abs   Expr          -- Absolute value
-- | Sqrt  Double Expr   -- Square root
-- | Gcd   Expr Expr     -- Greates commond divider

-- PLAN : 
-- Dus we hebben 1 kuifje programma, die dus geschreven is in een prgrammeer taal "kuifje"
-- we stringfyen die programma in haskell
-- het geeft een error terug on de command line ofzo, als dat niet lukt
-- we parsen die programma in haskell
-- en we evalueren en de resultaat is een distrubitie van moglijke uitkomsten. 
-- die uitkomst wordt ook mooi gevisualiseert en andere metrieken over de distributie worden ook 
-- getoont. 


-- initieel plan 1:  
-- we krijgen niks van input,want het kuifje programma's specifieert al reeds de nodige variable,n 
-- en bij iedere assignment en operatie introduceren we perturbaties in alle variableen. 
-- Het progamma stelt eigenlijk voor een wetenschappenlijk rekenmachine. 
-- wat er wordt geoutput is eignlijk de histogrammen van elk variable en hun metrieken. 

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
--   -- Observe f p <> k = Observe f (p <> k)

-- instance Monoid (Kuifje s) where
--   mempty = Skip
--   mappend = (<>)