{-# LANGUAGE Arrows, TemplateHaskell, 
             ExistentialQuantification, FlexibleContexts, 
             ScopedTypeVariables #-}

module Euterpea.IO.Audio.Basics (
  outA, 
  integral,
  countDown, countUp,
  upsample,
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


outA :: (ArrowInit a) => a b b
outA = arr' [| id |] id

integral :: (ArrowInitP a p, Clock p) => ArrowP a p Double Double
integral = 
    let dt = 1 / rate (undefined :: p)
    in proc x -> do
      rec let i' = i + x * dt
          i <- init 0 -< i'
      outA -< i

countDown :: ArrowInit a => Int -> a () Int
countDown x = 
    proc _ -> do
      rec 
          i <- init x -< i - 1
      outA -< i

countUp :: ArrowInit a => a () Int
countUp = 
    proc _ -> do
      rec 
         i <- init 0 -< i + 1
      outA -< i


upsample :: (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
             Clock p2, AudioSample b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsample f = g 
   where g = proc x -> do 
               rec
                 cc <- init 0 -< if cc >= r-1 then 0 else cc+1
                 y <- if cc == 0 then ArrowP (strip f) -< x 
                                 else init zero        -< y
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