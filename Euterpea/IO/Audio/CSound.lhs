> {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

This module is strictly for backward compatibility with Euterpea 0.1.0,
which used many csound names for the basic signal functions.

> module Euterpea.IO.Audio.CSound where
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Basics

> gen05    = tableExponN
> gen05'   = tableExpon
> gen07    = tableLinearN
> gen07'   = tableLinear
> gen09    = tableSines3N
> gen09'   = tableSines3
> gen10    = tableSinesN
> gen10'   = tableSines
> gen12    = tableBesselN
> gen12'   = tableBessel

> compSine1    = tableSinesN
> compSine2    = tableSines3N
> exponential1 = tableExponN
> lineSeg1     = tableLinearN

> tone     = filterLowPass 
> --             :: forall p . Clock p => Signal p (Double, Double) Double
> atone    = filterHighPass
> --             :: forall p . Clock p => Signal p (Double, Double) Double
> reson    = filterBandPass
> --             :: forall p . Clock p =>
> --                  Int -> Signal p (Double, Double, Double) Double
> areson   = filterBandStop
> --             :: forall p . Clock p =>
> --                  Int -> Signal p (Double, Double, Double) Double
> butterlp = filterLowPassBW
> butterhp = filterHighPassBW
> butterbp = filterBandPassBW
> butterbr = filterBandStopBW
> comb     = filterComb

> oscil    = osc
> oscili   = oscI
> oscils f = proc a -> do
>              o <- oscFixed f -< ()
>              outA -< o*a
> oscil1 tab del dur = 
>            proc a -> do
>              o <- oscDur  tab del dur -< ()
>              outA -< o*a
> oscil1i tab del dur =
>            proc a -> do
>              o <- oscDurI tab del dur -< ()
>              outA -< o*a

> buzz     = oscPartials

> -- pluck    = pluck
> -- balance  = balance

> line a d b =
>   proc s -> do
>     o <- envLine a d b -< ()
>     outA -< o*s

> expon a d b =
>   proc s -> do
>     o <- envExpon a d b -< ()
>     outA -< o*s

> linseg   = envLineSeg
> expseg   = envExponSeg

> linen rise dur dec = 
>   proc s -> do
>     o <- envASR rise dur dec -< ()
>     outA -< o*s

> envlpx rise dur dec tab atss atdec =
>   proc s -> do
>     o <- envCSEnvlpx rise dur dec tab atss atdec -< ()
>     outA -< o*s

> rand s = 
>   proc a -> do
>     o <- noiseWhite s -< ()
>     outA -< o*a

> randi s = 
>   proc (a,f) -> do
>     o <- noiseBLI s -< f
>     outA -< o*a

> randh s =
>   proc (a,f) -> do
>     o <- noiseBLH s -< f
>     outA -< o*a

> delay  = delayLine
> vdelay = delayLine1
> delay1 = delayLine1
> delayT = delayLineT
