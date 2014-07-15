{-# LANGUAGE Arrows, TemplateHaskell, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables #-}

-- The universal quantification (forall) for all type signatures has been 
-- commented out to allow the cca preprocessor to work properly.

module Euterpea.IO.Audio.Basics (
  outA, 
  integral,
  countDown, countUp,
  upsampleH, upsampleH', upsampleI, upsample,
  pchToHz, apToHz
) where

import Prelude hiding (init)

import Euterpea.Music.Note.Music
import Euterpea.IO.Audio.Types
import Control.Arrow
import Control.CCA.ArrowP
import Control.CCA.Types
import Control.CCA.CCNF

import Language.Haskell.TH
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Syntax

-- | An arrow identity.
outA :: {- forall a b . -} (ArrowInit a) => a b b
outA = arr' [| id |] id

-- | An integral that uses the clockrate of the clocked arrow to integrate.
integral :: {- forall a p . -} (ArrowInitP a p, Clock p) => ArrowP a p Double Double
integral = proc x -> do
    rec let i' = i + x * dt
        i <- init 0 -< i'
    outA -< i
  where dt = 1 / rate (undefined :: p)

-- | An arrow that counts down indefinitely starting from the supplied Int.
countDown :: {- forall a . -} ArrowInit a => Int -> a () Int
countDown x = proc _ -> do
    rec i <- init x -< i - 1
    outA -< i

-- | An arrow that counts up indefinitely starting from 0.
countUp :: {- forall a . -} ArrowInit a => a () Int
countUp = proc _ -> do
    rec i <- init 0 -< i + 1
    outA -< i

-- | The upsampleH function is an unsampler that holds (hence "H") the 
-- upsampled values for as many time steps as necessary.  Essentially, 
-- it turns the given arrow into a step function at its new clockrate.
-- To have consistent behavior with upsampleI, upsampleH applies a 
-- unit delay to the underlying lower sampled arrow.
upsampleH :: {- forall a p1 p2 x b. -} (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
              Clock p2, AudioSample b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsampleH f = proc x -> do 
    rec cc <- init 0 -< if cc >= r-1 then 0 else cc+1
      -- Note that the "init zero" in the next line prevents upsampling 
      -- from being instantaneous.  It is not strictly necessary here, 
      -- but it is in place to match the behavior of upsampleI.
    periodicProbe zero (init zero <<< ArrowP (strip f)) -< (x,cc == 0)
  where r = getUpsampleR (undefined :: p1) (undefined :: p2)

-- | The upsampleH' function is identical to the upsampleH function except 
-- that it does not apply a unit init.
upsampleH' :: {- forall a p1 p2 x b. -} (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
              Clock p2, AudioSample b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsampleH' f = proc x -> do 
    rec cc <- init 0 -< if cc >= r-1 then 0 else cc+1
    periodicProbe zero (ArrowP (strip f)) -< (x,cc == 0)
 where r = getUpsampleR (undefined :: p1) (undefined :: p2)


-- | The upsampleI function is an unsampler that interpolates (hence "I") 
-- the upsampled values.  A unit delay is applied so that the interpolation 
-- starts from zero and the first value is a unit of time late.
-- Note that interpolating requires that the output type be Fractional.  
-- 
upsampleI :: {- forall a p1 p2 x b. -} (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
              Clock p2, AudioSample b, Fractional b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsampleI f = proc x -> do 
    rec cc <- init 0 -< if cc >= r-1 then 0 else cc+1
    now  <- periodicProbe zero (ArrowP (strip f)) -< (x,   cc == 0)
    prev <- periodicProbe zero (init zero)       -< (now, cc == 0)
    outA -< prev + (now-prev)*(realToFrac $ cc/r)
  where r = getUpsampleR (undefined :: p1) (undefined :: p2)

{-# DEPRECATED upsample "Use upsampleH (or upsampleI) instead" #-}
upsample :: {- forall a p1 p2 b x . -} (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
             Clock p2, AudioSample b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsample = upsampleH


getUpsampleR :: {- forall p1 p2 . -} (Clock p1, Clock p2) => p1 -> p2 -> Double
getUpsampleR p1 p2 = if outRate < inRate 
    then error $ "Cannot upsample a signal of higher rate (" ++ show inRate
                 ++ ") to lower rate (" ++ show outRate ++ ")"
    else outRate / inRate 
  where inRate  = rate p1
        outRate = rate p2


-- Some useful auxiliary functions.

-- | The periodic probe function takes as arguments an initial value and a 
-- arrow to periodically probe.  When the streaming boolean value is True, 
-- the given arrow is provided with the streaming input, and otherwise it 
-- is left untouched.  The return stream is essentially a step function of 
-- the given arrow.
periodicProbe :: {- forall a x y . -} (ArrowChoice a, ArrowInit a) => y -> a x y -> a (x, Bool) y
periodicProbe i a = proc (x,b) -> do
  rec y <- if b then a -< x
                else (arr . const) i -< ()
      t1 <- init i -< if b then y else t1
  outA -< if b then y else t1



-- | Converting an AbsPitch to hertz (cycles per second):
apToHz :: {- forall a . -} Floating a => AbsPitch -> a
apToHz ap = 440 * 2 ** (fromIntegral (ap - absPitch (A,5)) / 12)

-- | Converting from a Pitch value to Hz:
pchToHz :: {- forall a . -} Floating a => Pitch -> a
pchToHz = apToHz . absPitch