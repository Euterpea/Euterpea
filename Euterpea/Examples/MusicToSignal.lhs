> {-# LANGUAGE Arrows #-}

This file demonstrates how to turn a Music value into an audio signal 
using the Render module.

> module Euterpea.Examples.MusicToSignal where

> import Euterpea

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

This stuff is taken from Euterpea.Examples.Interlude:

> bassLine =  timesM 3 b1 :+: timesM 2 b2 :+: 
>             timesM 4 b3 :+: timesM 5 b1

> mainVoice = timesM 3 v1 :+: v2

> v1   = v1a :+: graceNote (-1) (d 5 qn) :+: v1b                 -- bars 1-2
> v1a  = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
> v1b  = addDur en [cs 5, b 4]

> v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g
> v2a  =  line [  cs 5 (dhn+dhn), d 5 dhn, 
>                 f 5 hn, gs 5 qn, fs 5 (hn+en), g 5 en]
> v2b  =  addDur en [  fs 5, e 5, cs 5, as 4] :+: a 4 dqn :+:
>         addDur en [  as 4, cs 5, fs 5, e 5, fs 5]
> v2c  =  line [  g 5 en, as 5 en, cs 6 (hn+en), d 6 en, cs 6 en] :+:
>         e 5 en :+: enr :+: 
>         line [  as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en] 
> v2d  =  addDur en [  fs 5, cs 5, e 5, cs 5, 
>                      a 4, as 4, d 5, e 5, fs 5]
> v2e  =  line [  graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en,
>                 graceNote 1 (cs 5 qn), b 4 (en+hn), cs 5 en, b 4 en ]  
> v2f  =  line [  fs 5 en, a 5 en, b 5 (hn+qn), a 5 en, fs 5 en, e 5 qn,
>                 d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn]
> v2g  =  tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+: 
>         b 4 (3*dhn+hn)

> b1  = addDur dqn [b 3,   fs 4,  g 4,   fs 4]
> b2  = addDur dqn [b 3,   es 4,  fs 4,  es 4]
> b3  = addDur dqn [as 3,  fs 4,  g 4,   fs 4]

> addDur       :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)

> graceNote :: Int -> Music Pitch -> Music Pitch
> graceNote n  (Prim (Note d p))  =
>           note (d/8) (trans n p) :+: note (7*d/8) p
> graceNote n  _                  = 
>           error "Can only add a grace note to a note."

childSong6 :: Music Pitch
childSong6 =  let t = (dhn/qn)*(69/120)
              in instrument  RhodesPiano 
                             (tempo t (bassLine :=: mainVoice))
