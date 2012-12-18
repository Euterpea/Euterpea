> {-# LANGUAGE Arrows #-}

This file demonstrates how to turn a Music value into an audio signal 
using the Render module.

> module Euterpea.Examples.MusicToSignal where

> import Euterpea -- hiding (line,delay)
> -- import Euterpea.IO.Audio.Basics
> -- import Euterpea.IO.Audio.BasicSigFuns
> -- import Euterpea.IO.Audio.Types
> -- import Euterpea.IO.Audio.IO
> -- import Euterpea.IO.Audio.Render

> import Euterpea.Examples.Interlude (bassLine, mainVoice)

First, define some instruments.

> reedyWav = tableSinesN 1024 [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15, 
>                            0.0, 0.02, 0.05, 0.03]

> reed :: Instr (Stereo AudRate)
> reed dur pch vol params = 
>     let reedy = osc reedyWav 0
>         freq  = apToHz pch
>         vel   = fromIntegral vol / 127 / 3
>         env   = envLineSeg [0, 1, 0.8, 0.6, 0.7, 0.6, 0] 
>                            (replicate 6 (fromRational dur/6))
>     in proc _ -> do
>       amp <- env -< ()
>       r1 <- reedy -< freq
>       r2 <- reedy -< freq + (0.023 * freq)
>       r3 <- reedy -< freq + (0.019 * freq)
>       let [a1, a2, a3] = map (* (amp * vel)) [r1, r2, r3]
>       let rleft = a1 * 0.5 + a2 * 0.44 * 0.35 + a3 * 0.26 * 0.65
>           rright = a1 * 0.5 + a2 * 0.44 * 0.65 + a3 * 0.26 * 0.35
>       outA -< (rleft, rright)

> saw = tableSinesN 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
>                         0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

> plk :: Instr (Stereo AudRate)
> plk dur pch vol params = 
>     let vel  = fromIntegral vol / 127 / 3
>         freq = apToHz pch
>         sf   = pluck saw freq SimpleAveraging
>     in proc _ -> do
>          a <- sf -< freq
>          outA -< (a * vel * 0.4, a * vel * 0.6)

Define some instruments:

> myBass, myReed :: InstrumentName
> myBass = Custom "pluck-like"
> myReed = Custom "reed-like"

Construct a custom instrument map.  An instrument map is just 
an association list containing mappings from InstrumentName to Instr.

> myMap :: InstrMap (Stereo AudRate)
> myMap = [(myBass, plk), (myReed, reed)]

> bass   = mMap (\p-> (p, 40 :: Volume)) $ instrument myBass bassLine
> melody = mMap (\p-> (p,100 :: Volume)) $ instrument myReed mainVoice

> childSong6 :: Music (Pitch, Volume)
> childSong6 = tempo 1.5 (bass :=: melody)

All instruments used in the same performance must output the same number 
of channels, but renderSF supports both mono or stereo instruments 
(and any instrument that produces samples in the AudioSample type class).
The outFile function will produce a monaural or stereo file accordingly.

> recordSong = uncurry (outFile "song.wav") (renderSF childSong6 myMap)

> main = recordSong