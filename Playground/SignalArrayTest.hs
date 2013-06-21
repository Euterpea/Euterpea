module Main where
import Euterpea hiding (Signal, Table, tableSinesN, osc)
import Data.Array.Unboxed
import Data.WAVE
import System.CPUTime
import Text.Printf

sampleRate :: Int
sampleRate = 44100

newtype Signal a = Signal [a] deriving (Show)
runSignal :: Signal a -> [a]
runSignal (Signal s) = s

instance Num a => Num (Signal a) where
    (+) (Signal s1) (Signal s2) = Signal (zipWith (+) s1 s2)
    (-) (Signal s1) (Signal s2) = Signal (zipWith (-) s1 s2)
    (*) (Signal s1) (Signal s2) = Signal (zipWith (*) s1 s2)
    abs _ = error "abs unimplemented"
    signum _ = error "signum unimplemented"
    fromInteger _ = error "fromInteger unimplemented"

lift :: a -> Signal a
lift x = Signal (repeat x)

type Table = UArray Int Double

tableSinesN :: Int -> [Double] -> Table
tableSinesN size amps = 
    let wave x   = sum (zipWith (*) [sin (2*pi*x*n) | n <- [1..]] amps)
        delta    = 1 / fromIntegral size
        waveform = take size $ map wave [0,delta..]
        divisor  = (maximum . map abs) waveform
     in listArray (0,size) (map (/divisor) waveform)

tab1, tab2 :: Table
tab1 = tableSinesN 4096 [1]
tab2 = tableSinesN 4096 [1.0,0.5,0.33]

osc :: Table -> Double -> Signal Double -> Signal Double
osc table _ (Signal [])   = lift 0
osc table offset freq = 
    let (_,size) = bounds table
        deltas   = scanl1 (+) (map (/ fromIntegral sampleRate) (runSignal freq))
        idxs     = map ((`mod` size).round.(* fromIntegral size)) deltas
     in Signal (map (table !) (drop (round (fromIntegral size * offset)) idxs))

sineWave :: Double -> Signal Double
sineWave n = osc tab1 0 (lift n)

-- Example: vibrato 5 20 (lift 440)
vibrato :: Double -> Double -> Signal Double -> Signal Double
vibrato vibFreq depth sigIn =
    let vib = osc tab1 0 (lift vibFreq)
     in osc tab2 0 (sigIn + vib * lift depth)

signalToFile :: Int -> Signal Double -> IO ()
signalToFile n sig = do
    let samps  = map doubleToSample (take (n*sampleRate) (runSignal sig))
        frames = map (\x -> [x]) samps
        header = WAVEHeader 1 sampleRate 16 (Just (length frames))
     in putWAVEFile "out.wav" (WAVE header frames)

main = do
    t1 <- getCPUTime
    signalToFile 30 (vibrato 5 20 (lift 440))
    t2 <- getCPUTime
    let t = fromIntegral (t2-t1) * 1e-12 :: Double
    printf "Took %6.2fs\n" t
