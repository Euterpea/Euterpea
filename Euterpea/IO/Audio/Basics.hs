{-# LANGUAGE Arrows, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables #-}

module Euterpea.IO.Audio.Basics (
  outA, 
  integral,
  countDown, countUp,
  upsample,
  pchToHz, apToHz
) where

import Euterpea.Music.Note.Music
import Euterpea.IO.Audio.Types
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.ArrowP


outA :: (Arrow a) => a b b
outA = arr id

integral :: forall a p. (ArrowCircuit a, Clock p) => ArrowP a p Double Double
integral = 
    let dt = 1 / rate (undefined :: p)
    in proc x -> do
      rec let i' = i + x * dt
          i <- delay 0 -< i'
      outA -< i

countDown :: ArrowCircuit a => Int -> a () Int
countDown x = proc _ -> do
    rec i <- delay x -< i - 1
    outA -< i

countUp :: ArrowCircuit a => a () Int
countUp = proc _ -> do
    rec i <- delay 0 -< i + 1
    outA -< i


upsample :: forall a b c p1 p2. (ArrowChoice a, ArrowCircuit a, Clock p1, Clock p2, AudioSample c) 
         => ArrowP a p1 b c -> ArrowP a p2 b c
upsample f = g 
   where g = proc x -> do 
               rec
                 cc <- delay 0 -< if cc >= r-1 then 0 else cc+1
                 y <- if cc == 0 then ArrowP (strip f) -< x 
                                 else delay zero       -< y
               outA -< y
         r = if outRate < inRate 
             then error "Cannot upsample a signal of higher rate to lower rate" 
             else outRate / inRate
         inRate  = rate (undefined :: p1)
         outRate = rate (undefined :: p2)

-- Some useful auxiliary functions.

-- | Converting an AbsPitch to hertz (cycles per second):
apToHz :: Floating a => AbsPitch -> a
apToHz ap = 440 * 2 ** (fromIntegral (ap - absPitch (A,5)) / 12)

-- | Converting from a Pitch value to Hz:
pchToHz :: Floating a => Pitch -> a
pchToHz = apToHz . absPitch