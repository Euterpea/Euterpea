module Main where
import Euterpea hiding (Signal, Table, tableSinesN, osc, oscFixed, exportFile)
import Codec.Wav
import Data.Audio
import Data.Array.Unboxed
import Data.Int
import System.CPUTime
import Text.Printf

sr :: Int
sr = 44100

-- Signal Definition

newtype Signal a = Signal [a] deriving (Show)

-- Signal helpers

runSignal :: Signal a -> [a]
runSignal (Signal s) = s

sMap :: (a -> a) -> Signal a -> Signal a
sMap f (Signal s) = Signal (map f s)

sZipWith3 :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
sZipWith3 f (Signal s1) (Signal s2) (Signal s3) = Signal (zipWith3 f s1 s2 s3)

lift :: a -> Signal a
lift x = Signal (repeat x)

-- Signal instances

instance Num a => Num (Signal a) where
    (+) (Signal s1) (Signal s2) = Signal (zipWith (+) s1 s2)
    (-) (Signal s1) (Signal s2) = Signal (zipWith (-) s1 s2)
    (*) (Signal s1) (Signal s2) = Signal (zipWith (*) s1 s2)
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

osc :: Table -> Double -> Signal Double -> Signal Double
osc table _ sig = 
    let (_,size) = bounds table
        rate     = fromIntegral size / fromIntegral sr
        deltas   = scanl1 (+) (runSignal sig)
     in Signal (map ((table !).(`mod` size).round.(*rate)) deltas)

oscFixed :: Table -> Double -> Double -> Signal Double
oscFixed table _ freq = 
    let (_,size) = bounds table
        deltas   = [0,(freq * fromIntegral size / fromIntegral sr)..]
     in Signal (map ((table !).(`mod` size).round) deltas)

{- oscFixed :: Table -> Double -> Double -> Signal Double
oscFixed table offset freq = 
    let (_,size) = bounds table
        deltas   = [0,(freq / fromIntegral sr)..]
        idxs     = takeWhile (< size) (map (round.(* fromIntegral size)) deltas)
        (l,r)    = splitAt (round (fromIntegral size * offset)) idxs
     in Signal (cycle (map (table !) (r ++ l))) -}

sineWave :: Double -> Signal Double
sineWave n = oscFixed tab1 0 n

-- Write a signal to a file.

signalToFile :: String -> Double -> Signal Double -> IO ()
signalToFile filepath dur sf = 
  let numSamples  = truncate (dur * fromIntegral sr)
      dat         = map (fromSample . (*0.999)) (runSignal sf) :: [Int32]
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = sr,
                    channelNumber = 1,
                    sampleData    = array }
  in exportFile filepath aud

-- Example from "Plugging a Space Leak with an Arrow"

integralS :: Double -> Signal Double -> Signal Double
integralS i sig = Signal (scanl (+) i (map (/ fromIntegral sr) (runSignal sig)))

recursiveIntegral :: Signal Double
recursiveIntegral = integralS 1 recursiveIntegral

-- Translated examples from HSoM:

s1 :: Signal Double
s1 = sineWave 440

s2 :: Signal Double
s2 = osc tab1 0 (lift 440)

s3 :: Signal Double
s3 = oscFixed tab2 0 440

s4 :: Signal Double
s4 = let f0 = sineWave 440
         f1 = sineWave 880
         f2 = sineWave 1320
      in sZipWith3 (\a b c -> (a + 0.5*b + 0.33*c) / 1.83) f0 f1 f2

s5 :: Signal Double
s5 = vibrato 5 20 (lift 440)

-- Example from HSoM p.298

vibrato :: Double -> Double -> Signal Double -> Signal Double
vibrato vibFreq depth sigIn =
    let vib = osc tab1 0 (lift vibFreq)
     in osc tab2 0 (sigIn + vib * lift depth)

-- Example from HSoM p.300

simpleClip :: Signal Double -> Signal Double
simpleClip = sMap (\x -> if abs x <= 1.0 then x else signum x)

-- Custom instrument as described in HSoM p.302... can't render it yet, though

myInstr :: Instr (Signal Double)
myInstr dur ap vol [vfrq, dep] =
    let vib = osc tab1 0 (lift vfrq)
     in osc tab2 0 (lift (apToHz ap) + vib * lift dep)

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
