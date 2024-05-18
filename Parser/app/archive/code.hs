{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Kuifje.Distribution where

import Prelude hiding (null, filter, foldr, return, (>>=),fmap)
import Data.List (genericLength)
import Data.Semigroup
import Data.Map.Strict ( toList, elems)
import Data.Map

-- | Type synonym for probabilities.
type Prob = Rational

-- | Distribution data type.
newtype Dist a = D { runD :: Map a Prob }

-- | Type alias for hyper-distributions.
type Hyper a = Dist (Dist a) -- (a -> Prob) -> Prob

-- | Top-level fmap function for distributions.
fmap :: (Ord b) => (a -> b) -> Dist a -> Dist b
fmap f dx = dx >>= (return . f)

-- | Top-level return function for distributions. Creates a singleton
-- distribution.
return :: (Ord a) => a -> Dist a
return x = D $ singleton x 1

-- | Alias for return function.
point :: Ord a => a -> Dist a
point = return

-- | Top-level bind function for distributions.
(>>=) :: (Ord b) => Dist a -> (a -> Dist b) -> Dist b
d >>= f = D $ fromListWith (+) [(y, p * q) | (x, p) <- toList $ runD d, (y, q) <- toList $ runD (f x)]

-- | Top-level join function for distributions.
join :: (Ord a) => Hyper a -> Dist a
join x = x >>= id

instance Ord a => Eq (Dist a) where
  d1 == d2  =  unpackD d1 == unpackD d2

instance Ord a => Ord (Dist a) where
  d1 <= d2  =  unpackD d1 <= unpackD d2

-- | Construct a discrete distribution from a nonempty list of elements,
-- assigning the same probability to each element.
uniform :: (Ord a) => [a] -> Dist a
uniform l = D $ fromListWith (+) [(x, 1 / genericLength l) | x <- l]

-- | Construct a distribution in which the first element has probability p
-- and the second 1−p.
choose :: (Ord a) => Prob -> a -> a -> Dist a
choose p x y = D $ fromListWith (+) [(x, p), (y, 1 - p)]

-- | Recover the map representation of a distribution, reduced.
unpackD :: Dist a -> Map a Prob
unpackD = removeZeroes . runD
  where
    removeZeroes = filter (/= 0)

-- | Remove zeroes from a distribution.
reduction :: Dist a -> Dist a
reduction = D . unpackD

-- | Sum the probabilities in the distribution.
weight :: Dist a -> Prob
weight (D l) = foldr (+) 0 l


-- | Hyper-distribution type synonym.
type a ~~> b = Dist a -> Hyper b

-- | Bind with reduction applied to the input distribution.
(=>>) :: (Ord b) => Dist a -> (a -> Dist b) -> Dist b
m =>> f = reduction m >>= f

-- | Kleisli composition.
(==>) :: (Ord c) => (a ~> b) -> (b ~> c) -> (a ~> c)
f ==> g = \x -> f x >>= g

-- | For a given program, returns a function that calculates the
-- hyper-distribution for a given input distribution.
hysem :: (Ord s) => Kuifje s -> (s ~~> s)
hysem Skip          = return
hysem (Update f p)  = huplift f ==> hysem p
hysem (If c p q r)  = conditional c (hysem p) (hysem q) ==> hysem r
hysem (While c p q) = let wh = conditional c (hysem p ==> wh) (hysem q)
                      in wh
hysem (Observe f p) = hobsem f ==> hysem p

-- | Conditional semantics ('If' and 'While').
conditional :: (Ord s) => (s ~> Bool) -> (s ~~> s) -> (s ~~> s) -> (s ~~> s)
conditional c t e d
  = let d' = d =>> \s -> c s =>> \b -> return (b, s)
        w1 = sum [p | ((b, _), p) <- toList $ runD d', b]
        w2 = 1 - w1
        d1 = D $ fromListWith (+) [(s, p / w1) | ((b, s), p) <- toList $ runD d', b]
        d2 = D $ fromListWith (+) [(s, p / w2) | ((b, s), p) <- toList $ runD d', not b]
        h1 = t d1
        h2 = e d2
    in  if       null (runD d2)  then  h1
        else if  null (runD d1)  then  h2
                                 else  join (choose w1 h1 h2)

-- | Lifts a distribution to a hyper-distribution.
huplift :: (Ord s) => (s ~> s) -> (s ~~> s)
huplift f = return . (=>> f)

-- | 'Observe' semantics.
hobsem :: (Ord s, Ord o) => (s ~> o) -> (s ~~> s)
hobsem f = multiply . toPair . (=>> obsem f)
  where

    obsem :: (Ord o, Ord a) => (a ~> o) -> a ~> (o,a)
    obsem f' x = fmap (\w -> (w, x)) (f' x)

    toPair :: (Ord s, Ord o) => Dist (o, s) -> (Dist o, o -> Dist s)
    toPair dp = (d, f')
      where
        d     = fmap fst dp
        f' ws = let dpws = D $ fromListWith (+) [(s, p) | ((ws', s), p) <- toList $ runD dp, ws' == ws]
                in D $ fromListWith (+) [(s, p / weight dpws) | (s, p) <- toList $ runD dpws]

    multiply :: (Ord s) => (Dist o, o -> Dist s) -> Hyper s
    multiply (d, f') = fmap f' d

-- | Calculate Bayes Vulnerability for a distribution.
bayesVuln :: Ord a => Dist a -> Prob
bayesVuln = maximum . elems . runD . reduction

-- | Based on an entropy function for distributions, calculate the
-- average entropy for a hyper-distribution.
condEntropy :: (Dist a -> Rational) -> Hyper a -> Rational
condEntropy e m = average (fmap e m)

-- | Average a distribution of Rationals.
average :: Dist Rational -> Rational
average = sum . mapWithKey (*) . runD


-- | Kleisli arrow.
type a ~> b = a -> Dist b

-- | Syntax of the Kuifje language.
data Kuifje s
  = Skip
  | Update (s ~> s) (Kuifje s)
  | If (s ~> Bool) (Kuifje s) (Kuifje s) (Kuifje s)
  | While (s ~> Bool) (Kuifje s) (Kuifje s)
  | forall o. (Ord o) => Observe (s ~> o) (Kuifje s)

instance Semigroup (Kuifje s) where
  Skip        <> k = k
  Update f p  <> k = Update f (p <> k)
  While c p q <> k = While c p (q <> k)
  If c p q r  <> k = If c p q (r <> k)
  Observe f p <> k = Observe f (p <> k)

instance Monoid (Kuifje s) where
  mempty = Skip
  mappend = (<>)

-- | Return a 'Skip' instruction.
skip :: Kuifje s
skip = Skip

-- | Return an 'Update' instruction.
update' :: (s ~> s) -> Kuifje s
update' f = Update f skip

-- | Return a 'While' instruction.
while :: (s ~> Bool) -> Kuifje s -> Kuifje s
while c p = While c p skip

-- | Return an 'If' instruction.
cond :: (s ~> Bool) -> Kuifje s -> Kuifje s -> Kuifje s
cond c p q = If c p q skip

-- | Return an 'Observe' instruction.
observe :: (Ord o) => (s ~> o) -> Kuifje s
observe o = Observe o skip

