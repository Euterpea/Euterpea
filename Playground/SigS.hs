{-# LANGUAGE Arrows #-}
module SigS where
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

sSample :: Double -> SigS a -> SigS a
sSample sec (SigS sig) = SigS (take (truncate $ sec * fromIntegral sr) sig)

sDelay :: AudioSample a => Double -> SigS a -> SigS a
sDelay t (SigS sig) = SigS ((replicate (truncate $ t * fromIntegral sr) zero) ++ sig)

normalize :: SigS Double -> SigS Double
normalize (SigS sig) =
    let sig' = map (/ (maximum sig)) sig
     in (SigS sig')

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

-- Workaround for rendering. Only works when the sampling rate is 44100, which it is.

liftSF :: SigS Double -> AudSF () Double
liftSF (SigS sig) = proc () -> do
    rec (s:sig') <- hold sig -< Just sig'
    returnA -< s
