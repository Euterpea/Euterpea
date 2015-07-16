-- Filename: fft.hs
-- Created by: Daniel Winograd-Cort
-- Created on: unknown
-- Last Modified by: Daniel Winograd-Cort
-- Last Modified on: 12/12/2013

-- This module requires the array and pure-fft packages.

{-# LANGUAGE Arrows #-}
module Euterpea.IO.MUI.FFT where
import FRP.UISF
import Control.Arrow.Operations
import Numeric.FFT (fft)
import Data.Complex
import Data.Map (Map)
import qualified Data.Map as Map



-- | Alternative for working with Math.FFT instead of Numeric.FFT
--import qualified Math.FFT as FFT
--import Data.Array.IArray
--import Data.Array.CArray
--myFFT n lst = elems $ (FFT.dft) (listArray (0, n-1) lst)


--------------------------------------
-- Fast Fourier Transform
--------------------------------------

-- | Returns n samples of type b from the input stream at a time, 
--   updating after k samples.  This function is good for chunking 
--   data and is a critical component to fftA
quantize :: ArrowCircuit a => Int -> Int -> a b (SEvent [b])
quantize n k = proc d -> do
    rec (ds,c) <- delay ([],0) -< (take n (d:ds), c+1)
    returnA -< if c >= n && c `mod` k == 0 then Just ds else Nothing

-- | Converts the vector result of a dft into a map from frequency to magnitude.
--   One common use is:
--      fftA >>> arr (fmap $ presentFFT clockRate)
presentFFT :: Double -> [Double] -> Map Double Double
presentFFT clockRate a = Map.fromList $ zipWith (curry mkAssoc) [0..] a where 
    mkAssoc (i,c) = (freq, c) where
        samplesPerPeriod = fromIntegral (length a)
        freq = i * (clockRate / samplesPerPeriod)

-- | Given a quantization frequency (the number of samples between each 
--   successive FFT calculation) and a fundamental period, this will decompose
--   the input signal into its constituent frequencies.
--   NOTE: The fundamental period must be a power of two!
fftA :: ArrowCircuit a => Int -> Int -> a Double (SEvent [Double])
fftA qf fp = proc d -> do
    carray <- quantize fp qf -< d :+ 0
    returnA -< fmap (map magnitude . take (fp `div` 2) . fft) carray


