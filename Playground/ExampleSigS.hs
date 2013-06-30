module Main where
import Euterpea hiding (Table, tableSinesN, osc, oscFixed, exportFile)
import Euterpea.Examples.Interlude
import Codec.Wav
import Data.Audio
import Data.Array.Unboxed hiding (accum)
import Data.Int
import System.CPUTime
import Text.Printf
import SigS
import RenderSigS

-- Example from "Plugging a Space Leak with an Arrow"

integralS :: Double -> SigS Double -> SigS Double
integralS i sig = SigS (scanl (+) i (map (/ sr) (runSigS sig)))

recursiveIntegral :: SigS Double
recursiveIntegral = integralS 1 recursiveIntegral

-- Translated examples from HSoM:

s1 :: SigS Double
s1 = sineWave 440

s2 :: SigS Double
s2 = osc tab1 0 (lift 440)

s3 :: SigS Double
s3 = oscFixed tab2 0 440

s4 :: SigS Double
s4 = let f0 = sineWave 440
         f1 = sineWave 880
         f2 = sineWave 1320
      in sZipWith3 (\a b c -> (a + 0.5*b + 0.33*c) / 1.83) f0 f1 f2

s5 :: SigS Double
s5 = vibrato 5 20 (lift 440)

-- Example from HSoM p.298

vibrato :: Double -> Double -> SigS Double -> SigS Double
vibrato vibFreq depth sigIn =
    let vib = osc tab1 0 (lift vibFreq)
     in osc tab2 0 (sigIn + vib * lift depth)

-- Example from HSoM p.300

simpleClip :: SigS Double -> SigS Double
simpleClip = sMap (\x -> if abs x <= 1.0 then x else signum x)

-- Custom instrument as described in HSoM p.302.

myInstr :: Instr (SigS Double)
myInstr dur ap vol [vfrq, dep] =
    let vib = osc tab1 0 (lift vfrq)
     in osc tab2 0 (lift (apToHz ap) + vib * lift dep)
myInstr dur ap vol _ = osc tab2 0 (lift (apToHz ap))

myInstrName :: InstrumentName
myInstrName = Custom "signal-test"

myInstrMap :: InstrMap (SigS Double)
myInstrMap = [(myInstrName, myInstr), (RhodesPiano, myInstr)]

-- Number of seconds to sample in each test.
-- Higher values often expose space leaks.
numSeconds = 120.0

-- Samples numSeconds from test and saves the result to fname.wav
-- Reports time spent to stdout.
runTest fname test = do
    t1 <- getCPUTime
    signalToFile (fname ++ ".wav") numSeconds test
    t2 <- getCPUTime
    printf "Calculating %s took %4.2fs\n" fname (fromIntegral (t2-t1) * 1e-12 :: Double)

-- Renders mus using myInstrMap and saves the result to fname.wav
-- Reports time spent to stdout.
runTestI fname mus = do
    t1 <- getCPUTime
    let (ds,sf) = renderSigS mus myInstrMap
    signalToFile (fname ++ ".wav") ds sf
    t2 <- getCPUTime
    printf "Calculating %s took %4.2fs\n" fname (fromIntegral (t2-t1) * 1e-12 :: Double)

main = do
    t1 <- getCPUTime
    runTest "s1" s1
    runTest "s2" s2
    runTest "s3" s3
    runTest "s4" s4
    runTest "s5" s5
    runTestI "cs6" childSong6
    runTestI "mel" mel
    t2 <- getCPUTime

    printf "Total time was:     %4.2fs\n" (fromIntegral (t2-t1) * 1e-12 :: Double)

-- Example melody:

mel :: Music1
mel = let m = Euterpea.line [na1 (c 4 en),na1 (ef 4 en),na1 (f 4 en),
                             na2 (af 4 qn),na1 (f 4 en), na1 (af 4 en),
                             na2 (bf 4 qn), na1 (af 4 en),na1 (bf 4 en),
                             na1 (c 5 en), na1 (ef 5 en), na1 (f 5 en),
                             na3 (af 5 wn)]
          na1 (Prim (Note d p)) = Prim (Note d (p,[Params [0,0]]))
          na2 (Prim (Note d p)) = Prim (Note d (p,[Params [5,10]]))
          na3 (Prim (Note d p)) = Prim (Note d (p,[Params [5,20]]))
       in instrument myInstrName m