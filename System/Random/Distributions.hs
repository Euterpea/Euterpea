-- Algorithms taken from Dodge and Jerse's Computer Music: Synthesis,
-- Composition, and Performance, Chapter 11. 

module System.Random.Distributions (
  -- * Random Distributions
  linear, exponential, bilExp, gaussian, cauchy, poisson, frequency

  -- * Utility Functions
  , rands

  ) where

import System.Random

{- | Given a random number generator, generates a linearly distributed
random variable between 0 and 1.  Returns the random value together
with a new random number generator.  The probability density function
is given by

> f(x) = 2(1-x)  0 <= x <= 1
>      = 0       otherwise

-}
linear :: (RandomGen g, Floating a, Random a, Ord a) => g -> (a,g)
linear g0 = 
    let (r1, g1) = randomR (0, 1) g0
        (r2, g2) = randomR (0, 1) g1
    in (min r1 r2, g2)

{- | Takes a random number generator and produces another one
 that avoids generating the given number.
-}
avoid :: (Random a, Eq a, RandomGen g) => a -> (g -> (a,g)) -> g -> (a,g)
avoid x f g = if r == x then avoid x f g' else (r,g')
    where (r,g') = f g

{- | Generates an exponentially distributed random variable given a
spread parameter lambda.  A larger spread increases the probability of
generating a small number.  The mean of the distribution is
1/lambda.  The range of the generated number is [0,inf] although
the chance of getting a very large number is very small.

The probability density function is given by

> f(x) = lambda e^(-lambda * x)
-}
exponential :: (RandomGen g, Floating a, Random a, Eq a) => 
            a  -- ^ horizontal spread of the function.
         -> g  -- ^ a random number generator.
         -> (a,g)
exponential lambda g0 = (-log r1 / lambda, g1)
    where (r1, g1) = avoid 0 random g0

{- | Generates a random number with a bilateral exponential distribution.  
Similar to exponential, but the mean of the distribution is 0 and
50% of the results fall between (-1/lambda, 1/lambda).

-}
bilExp :: (Floating a, Ord a, Random a, RandomGen g) =>
          a    -- ^ horizontal spread of the function.
       -> g    -- ^ a random number generator.
       -> (a,g)
bilExp lambda g0 = 
    let (r', g1) = avoid 0 random g0
        r = 2 * r'
        u = if r > 1 then 2 - r else r
    in (signum (1 - r) * log u / lambda, g1)

{- | Generates a random number with a Gaussian distribution.  
-}
gaussian :: (Floating a, Random a, RandomGen g) =>
            a     -- ^ standard deviation.
         -> a     -- ^ mean.
         -> g     -- ^ a random number generator.
         -> (a,g)
gaussian stddev center g0 = 
    let n = 12
        s = sum $ take n $ randoms g0
    in (stddev * (s - fromIntegral n / 2) + center, fst (split g0))

{- | Generates a Cauchy-distributed random variable.  
The distribution is symmetric with a mean of 0.  

-}
cauchy :: (Floating a, Random a, RandomGen g, Eq a) =>
          a   -- ^ alpha (density).
       -> g   -- ^ a random number generator.
       -> (a,g)
cauchy density g0 = (density * tan (u * pi), g1)
    where (u, g1) = avoid 0.5 random g0

{- | Generates a Poisson-distributed random variable.
The given parameter lambda is the mean of the distribution.  
If lambda is an integer, the probability that the result j=lambda-1
will be as great as that of j=lambda.  The Poisson distribution
is discrete. The returned value will be a non-negative
integer.

-}
poisson :: (Num t, Ord a, Floating a, RandomGen g, Random a) =>
           a -> g -> (t, g)
poisson lambda g0 = (k 0 us, g1)
    where v   = exp (-lambda)
          us  = scanl1 (*) (randoms g0)
          g1  = fst (split g0)
          k n (u:us)
              | u >= v     = k (n+1) us
              | otherwise  = n
          k _ [] = error "System.Random.Distributions.poisson: randoms did not return an infinite list"

{- | Given a list of weight-value pairs, generates a value randomly picked
from the list, weighting the probability of choosing each value by the 
weight given.

-}
frequency :: (Floating w, Ord w, Random w, RandomGen g) 
             => [(w, a)] -> g -> (a,g)
frequency xs g0 = (pick r xs, g1)
    where (r, g1) = randomR (0, tot) g0
          tot = sum (map fst xs)
          pick n ((w,a):xs) 
             | n <= w    = a
             | otherwise = pick (n-w) xs
          pick _ [] = error "System.Random.Distributions.frequency: The impossible happened"


{- | Given a function generating a random number variable and a random
number generator, produces an infinite list of random values 
generated from the given function.

-}
rands :: (RandomGen g, Random a) => 
         (g -> (a,g)) -> g -> [a]
rands f g = x : rands f g' where (x,g') = f g

