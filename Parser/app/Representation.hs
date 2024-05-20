module Representation (Kuifje(..), Statement(..), Expr(..), Cond(..), Comp(..), Variable,
    ContDist(..), DscrDist(..), skip, returns, update, while, cond ) where
import qualified Data.Map as M

type Variable = String
data Statement = Stat Variable Expr

instance Show Statement where
    show (Stat s k1) = s ++ " = " ++ show k1 ++ ";"


data Expr = Add   Expr Expr ContDist
          | Sub   Expr Expr ContDist
          | Mul   Expr Expr ContDist
          | Div   Expr Expr ContDist
          | Mod   Expr Expr ContDist -- modulo operator. 
          | Lit   Int ContDist
          | Var   Variable -- eerder gedeclareerde variablen kunnen hier gebruikt worden.

instance Show Expr where
  show (Add   e1 e2 c)  = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Sub   e1 e2 c)  = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
  show (Mul   e1 e2 c)  = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
  show (Div   e1 e2 c)  = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (Mod   e1 e2 c)  = "(" ++ show e1 ++ "%" ++ show e2 ++ ")"
  show (Lit   i c)      = show i
  show (Var   v)        = v

data Cond = And Cond Cond DscrDist
          | Or  Cond Cond DscrDist
          | Comp Comp

instance Show Cond where
  show (And  c1 c2 d)               = "(" ++ show c1 ++ " && "  ++ show c2  ++ ")"
  show (Or   c1 c2 d)               = "(" ++ show c1 ++ " || "  ++ show c2  ++ ")"
  show (Comp c      )               = "(" ++ show c  ++ ")"

data Comp = Equal        Expr Expr DscrDist
          | NotEqual     Expr Expr DscrDist
          | LessThan     Expr Expr DscrDist
          | GreaterThan  Expr Expr DscrDist
          | LessThanOrEqual    Expr Expr DscrDist
          | GreaterThanOrEqual Expr Expr DscrDist

instance Show Comp where
  show (Equal        e1 e2 d)       = "( " ++ show e1 ++ " == "  ++ show e2  ++ " )"
  show (NotEqual     e1 e2 d)       = "( " ++ show e1 ++ " != "  ++ show e2  ++ " )"
  show (LessThan     e1 e2 d)       = "( " ++ show e1 ++ " < "   ++ show e2  ++ " )"
  show (GreaterThan  e1 e2 d)       = "( " ++ show e1 ++ " > "   ++ show e2  ++ " )"
  show (LessThanOrEqual    e1 e2 d) = "( " ++ show e1 ++ " <= "  ++ show e2  ++ " )"
  show (GreaterThanOrEqual e1 e2 d) = "( " ++ show e1 ++ " >= "  ++ show e2  ++ " )"

type Mu     = Double
type Sigma  = Double
type Alpha  = Double
type Beta   = Double
type UpperBound = Double
type LowerBound = Double

data ContDist = Beta      Alpha Beta
              | Gaussian  Mu    Sigma
              | Uniform     LowerBound UpperBound
              | Exponential Double
              | Poisson     Double

instance Show ContDist where
  show (Beta     a b    ) = "Beta "         ++ show a ++ " " ++ show b
  show (Gaussian m s    ) = "Gaussian "     ++ show m ++ " " ++ show s
  show (Uniform  a b    ) = "Beta "         ++ show a ++ " " ++ show b
  show (Exponential lmbd) = "Exponential "  ++ show lmbd
  show (Poisson r       ) = "Poisson "      ++ show r

data DscrDist = Bernoulli   Double

instance Show DscrDist where
  show (Bernoulli p) = "Bernoulli " ++ show p


data Kuifje
  = Skip
  | Return  Variable
  | Update  Statement Kuifje
  | If      Cond Kuifje Kuifje Kuifje
  | While   Cond Kuifje Kuifje

instance Show Kuifje where
  show  Skip                = ""
  show (Return  a        )  = "return " ++ a ++ ";\n"
  show (Update s k1      )  = show s ++ "\n" ++ show k1
  show (If     c k1 k2 k3)  = "if (" ++ show c ++ "){\n" ++
                                  show k1 ++ "}\n" ++ 
                              "else (" ++ show k2 ++ "){\n" ++
                                  show k3 ++ "}\n" 

  show (While  c k1 k2   )  = "while (" ++ show c ++ "){\n" ++ 
                                  show k1 ++ "}\n" ++ 
                                  show k2
instance Semigroup Kuifje where
  Return a      <> l    = Return a -- FIXME? 
  Skip          <> l    = l
  Update s p    <> l    = Update s (p <> l)
  While c p q   <> l    = While c p (q <> l)
  If c p q r    <> l    = If c p q (r <> l)

instance Monoid Kuifje where
  mempty = Skip
  mappend = (<>)

skip :: Kuifje
skip = Skip

update :: Statement -> Kuifje
update f = Update f skip

while :: Cond -> Kuifje -> Kuifje -> Kuifje
while = While

cond :: Cond -> Kuifje -> Kuifje -> Kuifje -> Kuifje
cond = If

returns :: Variable -> Kuifje
returns = Return