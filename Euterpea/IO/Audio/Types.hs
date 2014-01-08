{-# LANGUAGE TypeOperators, EmptyDataDecls, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleInstances,
             ScopedTypeVariables #-}

module Euterpea.IO.Audio.Types where

import Control.CCA.Types
import Control.CCA.CCNF
import Control.CCA.ArrowP
import Control.SF.SF


class Clock p where
    rate :: p -> Double  -- sampling rate

data AudRate
data CtrRate

instance Clock AudRate where
    rate _ = 44100

instance Clock CtrRate where
    rate _ = 4410

type AudSF a b  = SigFun AudRate a b
type CtrSF a b  = SigFun CtrRate a b

type Signal clk a b    = ArrowP SF clk a b
type SigFun clk a b    = ArrowP SF clk a b
type SignalSyn clk a b = ArrowP ASyn clk a b

-- Arbitrary number of channels (say, 5.1) can be supported by just adding more
-- instances of the AudioSample type class.

class AudioSample a where
    zero :: a
    mix :: a -> a -> a
    collapse :: a -> [Double]
    numChans :: a -> Int   
      -- allows us to reify the number of channels from the type.

instance AudioSample Double where
    zero = 0
    mix = (+)
    collapse a = [a]
    numChans _ = 1

instance AudioSample (Double,Double) where
    zero = (0,0)
    mix (a,b) (c,d) = (a+c,b+d)
    collapse (a,b) = [a,b]
    numChans _ = 2

-- Some useful type synonyms:
type Mono p = Signal p () Double
type Stereo p = Signal p () (Double,Double)

-- Experimental stuff
class Unlifted a where
    expose :: a -> b -> b
    expose = seq
    unlifted_dummy :: a
    unlifted_dummy = error "unlifted_dummy"

instance Unlifted Double
instance Unlifted Float
instance Unlifted Int
instance Unlifted ()
instance Unlifted a => Unlifted [a]

instance (Unlifted a, Unlifted b) => Unlifted (a -> b)

data (Unlifted a, Unlifted b) => a :!: b = !a :!: !b
instance (Unlifted a, Unlifted b) => Unlifted (a :!: b) where
  expose (a :!: b) s = expose a (expose b s)
  {-# INLINE expose #-}

instance (Unlifted a, Unlifted b) => Unlifted (a,b) where
    expose (a, b) s = expose a (expose b s)
    {-# INLINE expose #-}

instance (Unlifted a, Unlifted b, Unlifted c) => Unlifted (a,b,c) where
    expose (a, b, c) s = expose a (expose b (expose c s))
    {-# INLINE expose #-}

instance (Unlifted a, Unlifted b, Unlifted c, Unlifted d) 
    => Unlifted (a,b,c,d) where
    expose (a, b, c,d) s = expose a (expose b (expose c (expose d s)))
    {-# INLINE expose #-}

instance (Unlifted a, Unlifted b, Unlifted c, Unlifted d, Unlifted e) 
    => Unlifted (a,b,c,d,e) where
    expose (a, b, c,d,e) s = expose a (expose b (expose c (expose d (expose e s))))
    {-# INLINE expose #-}
