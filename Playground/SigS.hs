{-# LANGUAGE Arrows #-}
module SigS where
import Euterpea hiding (Table, tableSinesN, osc, oscFixed, exportFile)
import Euterpea.Music.Signal.SigFuns (AudSF)
import Codec.Wav
import Data.Audio
import Data.Array.Unboxed
import Data.List
import Data.Int
import System.CPUTime
import Text.Printf

sr :: Double
sr = rate (undefined :: AudRate)

-- SigS Definition

newtype SigS a = SigS [a] deriving (Show)

-- SigS helpers

runSigS :: SigS a -> [a]
runSigS (SigS s) = s

sMap :: (a -> a) -> SigS a -> SigS a
sMap f (SigS s) = SigS (map f s)

sZipWith :: (a -> b -> c) -> SigS a -> SigS b -> SigS c
sZipWith f (SigS s1) (SigS s2) = SigS (zipWith f s1 s2)

sZipWith3 :: (a -> b -> c -> d) -> SigS a -> SigS b -> SigS c -> SigS d
sZipWith3 f (SigS s1) (SigS s2) (SigS s3) = SigS (zipWith3 f s1 s2 s3)

sSample :: Double -> SigS a -> SigS a
sSample sec (SigS sig) = SigS (take (truncate $ sec * sr) sig)

sDelay :: AudioSample a => Double -> SigS a -> SigS a
sDelay t (SigS sig) = SigS (replicate (truncate $ t * sr) zero ++ sig)


-- Use this to lift a constant value to the SigS level.
lift :: a -> SigS a
lift x = SigS (repeat x)

-- SigS instances

instance Num a => Num (SigS a) where
    (+) = sZipWith (+)
    (-) = sZipWith (-)
    (*) = sZipWith (*)
    abs = sMap abs
    signum = sMap signum
    fromInteger = lift . fromInteger

-- Table definition

type Table = UArray Int Double

-- Sine table generator. Takes an integer representing the number of samples to generate
-- and a list of relative intensities for the overtones of the sine wave.

tableSinesN :: Int -> [Double] -> Table
tableSinesN size amps = 
    let wave x   = sum (zipWith (*) [sin (2*pi*x*n) | n <- [1..]] amps)
        delta    = 1 / fromIntegral size
        waveform = take size $ map wave [0,delta..]
        divisor  = (maximum . map abs) waveform
     in listArray (0,size) (map (/divisor) waveform)

-- Two example sine tables.

tab1, tab2 :: Table
tab1 = tableSinesN 4096 [1] -- Basic sine wave
tab2 = tableSinesN 4096 [1.0,0.5,0.33]

-- Table-driven oscillator. The second index should represent an offset into the table,
-- but hasn't been implemented yet. The signal is a signal of frequencies for the
-- oscillator (lift 440 would give an A, for example)

osc :: Table -> Double -> SigS Double -> SigS Double
osc table _ sig = 
    let (_,size) = bounds table
        rate     = fromIntegral size / sr
        deltas   = scanl1 (+) (runSigS sig)
     in SigS (map ((table !).(`mod` size).round.(*rate)) deltas)

-- More efficient specialization of osc for constant frequencies.

oscFixed :: Table -> Double -> Double -> SigS Double
oscFixed table _ freq = 
    let (_,size) = bounds table
        deltas   = [0,(freq * fromIntegral size / sr)..]
     in SigS (map ((table !).(`mod` size).round) deltas)

-- TODO: Reimplement Euterpea's envelope functions in the SigS paradigm.

-- Convenient synonym for oscFixed tab1 0.

sineWave :: Double -> SigS Double
sineWave = oscFixed tab1 0

-- Write a signal to a file.
-- Uses Codec.Wav

signalToFile :: String -> Double -> SigS Double -> IO ()
signalToFile filePath dur sig =
    let numSamples = truncate (dur * sr)
        dat = listArray (0,numSamples-1) (map (fromSample.(*0.999)) (runSigS sig)) :: UArray Int Int32
     in exportFile filePath Audio { sampleRate = truncate sr, channelNumber = 1, sampleData = dat }

-- Lifts SigS a's to the AudSF level for compatibility with the current arrow-based library.

liftSF :: AudioSample a => SigS a -> AudSF () a
liftSF (SigS sig) = proc () -> do
    rec (s:sig') <- hold sig -< Just sig'
    returnA -< s
