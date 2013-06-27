{-# LANGUAGE Arrows #-}
module Main where
import Control.Arrow
import Euterpea hiding (Table, tableSinesN, osc, oscFixed, exportFile)
import Euterpea.Music.Signal.SigFuns (AudSF)
import Codec.Wav
import Data.Audio
import Data.Array.Unboxed hiding (accum)
import Data.Int
import System.CPUTime
import Text.Printf

sr :: Int
sr = 44100

-- SigS Definition

newtype SigS a = SigS [a] deriving (Show)

-- SigS helpers

runSigS :: SigS a -> [a]
runSigS (SigS s) = s

sMap :: (a -> a) -> SigS a -> SigS a
sMap f (SigS s) = SigS (map f s)

sZipWith3 :: (a -> b -> c -> d) -> SigS a -> SigS b -> SigS c -> SigS d
sZipWith3 f (SigS s1) (SigS s2) (SigS s3) = SigS (zipWith3 f s1 s2 s3)

lift :: a -> SigS a
lift x = SigS (repeat x)

-- SigS instances

instance Num a => Num (SigS a) where
    (+) (SigS s1) (SigS s2) = SigS (zipWith (+) s1 s2)
    (-) (SigS s1) (SigS s2) = SigS (zipWith (-) s1 s2)
    (*) (SigS s1) (SigS s2) = SigS (zipWith (*) s1 s2)
    abs _ = error "abs unimplemented"
    signum _ = error "signum unimplemented"
    fromInteger _ = error "fromInteger unimplemented"

-- Table definition

type Table = UArray Int Double

-- Sine table generator

tableSinesN :: Int -> [Double] -> Table
tableSinesN size amps = 
    let wave x   = sum (zipWith (*) [sin (2*pi*x*n) | n <- [1..]] amps)
        delta    = 1 / fromIntegral size
        waveform = take size $ map wave [0,delta..]
        divisor  = (maximum . map abs) waveform
     in listArray (0,size) (map (/divisor) waveform)

-- Two example sine tables.

tab1, tab2 :: Table
tab1 = tableSinesN 4096 [1]
tab2 = tableSinesN 4096 [1.0,0.5,0.33]

-- Simple oscillators

osc :: Table -> Double -> SigS Double -> SigS Double
osc table _ sig = 
    let (_,size) = bounds table
        rate     = fromIntegral size / fromIntegral sr
        deltas   = scanl1 (+) (runSigS sig)
     in SigS (map ((table !).(`mod` size).round.(*rate)) deltas)

oscFixed :: Table -> Double -> Double -> SigS Double
oscFixed table _ freq = 
    let (_,size) = bounds table
        deltas   = [0,(freq * fromIntegral size / fromIntegral sr)..]
     in SigS (map ((table !).(`mod` size).round) deltas)

sineWave :: Double -> SigS Double
sineWave n = oscFixed tab1 0 n

-- Write a signal to a file.

signalToFile :: String -> Double -> SigS Double -> IO ()
signalToFile filepath dur sf = 
  let numSamples  = truncate (dur * fromIntegral sr)
      dat         = map (fromSample . (*0.999)) (runSigS sf) :: [Int32]
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = sr,
                    channelNumber = 1,
                    sampleData    = array }
  in exportFile filepath aud

-- Example from "Plugging a Space Leak with an Arrow"

integralS :: Double -> SigS Double -> SigS Double
integralS i sig = SigS (scanl (+) i (map (/ fromIntegral sr) (runSigS sig)))

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

myInstrName :: InstrumentName
myInstrName = Custom "signal-test"

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [(myInstrName, (\dur ap vol [vfrq, dep] -> liftSF (myInstr dur ap vol [vfrq, dep])))]

saveWithInstr :: Performable a => Music a -> IO ()
saveWithInstr mus = 
    let (dr,sf) = renderSF mus myInstrMap
     in outFile "instr.wav" dr sf

-- Workaround for rendering. Should be replaced with something like renderSigS.
-- Only works when the sampling rate is 44100, which it is.

liftSF :: SigS Double -> AudSF () Double
liftSF (SigS sig) = proc () -> do
    rec (s:sig') <- hold sig -< Just sig'
    returnA -< s

-- Main program. Benchmarks s1..s5.

numSeconds = 120.0

runTest fname test = do
    t1 <- getCPUTime
    signalToFile (fname ++ ".wav") numSeconds test
    t2 <- getCPUTime
    printf "Calculating %s took %4.2fs\n" fname (fromIntegral (t2-t1) * 1e-12 :: Double)

main = do
    t1 <- getCPUTime
    runTest "s1" s1
    runTest "s2" s2
    runTest "s3" s3
    runTest "s4" s4
    runTest "s5" s5
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