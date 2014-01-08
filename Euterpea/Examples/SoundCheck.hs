{-# LANGUAGE Arrows #-}

module Euterpea.Examples.SoundCheck where

import Euterpea

sineTable     = tableSinesN 16384 [1]
sawtoothTable = tableSinesN 16384 
                  [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.125, 0.111]

oscSine = osc sineTable 0

sine :: AudSF () Double
sine = 
    proc _ -> do
      oscSine -< 440

sine_am :: AudSF () Double
sine_am = 
    proc _ -> do
      amp  <- oscSine -< 5
      s    <- oscSine -< 440
      outA -< amp * s

sine_fm :: AudSF () Double
sine_fm = 
    proc _ -> do
      frq <- oscSine -< 3
      oscSine -< 330 + frq * 110 -- oscillates between 220 and 440 at 3 Hz

sine_fm2 :: AudSF () Double
sine_fm2 = 
    proc _ -> do
      modfrq <- oscSine -< 0.1
      frq    <- oscSine -< 3 + modfrq * 100
      oscSine -< 330 + frq * 110 -- oscillates between 220 and 440 at 3 Hz

sawtooth :: AudSF () Double
sawtooth = 
    proc _ -> do
      osc sawtoothTable 0 -< 440

squareWave :: AudSF () Double
squareWave =
    proc _ -> do
      frq <- oscSine -< 1000
      outA -< if frq > 0 then 0.99 else -0.99

test :: AudSF () Double -> IO ()
test = outFile "test.wav" 3.0
