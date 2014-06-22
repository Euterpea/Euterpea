{-# LANGUAGE BangPatterns, ExistentialQuantification, 
    ScopedTypeVariables, FlexibleContexts, Arrows #-}

module Euterpea.IO.Audio.IO (
    outFile,  outFileNorm, 
    playSignal, playSignalNorm,
 -- outFileA, outFileNormA, RecordStatus, 
    maxSample) where

import Prelude hiding (init)
import Euterpea.IO.Audio.Types hiding (Signal)
import Euterpea.IO.Audio.PortAudioChannel

import Control.CCA.ArrowP
import Control.Concurrent.MonadIO
import Control.Exception
import Control.Monad
import Control.SF.SF
import Codec.Wav
import Data.Array.Unboxed
import Data.Audio
import Data.Int

type Signal clk a b = ArrowP SF clk a b

-- | Writes sound to a wave file (.wav)
outFile :: forall a p. (AudioSample a, Clock p) => 
           String              -- ^ Filename to write to.
        -> Double              -- ^ Duration of the wav in seconds.
        -> Signal p () a       -- ^ Signal representing the sound.
        -> IO ()
outFile = outFileHelp id

normList :: [Double] -> [Double]
normList xs = map (/ mx) xs 
    where mx = max 1.0 (maximum (map abs xs))

-- | Like outFile, but normalizes the output if the amplitude of 
-- the signal goes above 1.  If the maximum sample is less than
-- or equal to 1, the output is not normalized.
-- Currently this requires storing the entire output stream in memory
-- before writing to the file.
outFileNorm :: forall a p. (AudioSample a, Clock p) => 
            String              -- ^ Filename to write to.
         -> Double              -- ^ Duration of the wav in seconds.
         -> Signal p () a       -- ^ Signal representing the sound.
         -> IO ()
outFileNorm = outFileHelp normList

outFileHelp :: forall a p. (AudioSample a, Clock p) => 
            ([Double] -> [Double]) -- ^ Post-processing function.
         -> String                 -- ^ Filename to write to.
         -> Double                 -- ^ Duration of the wav in seconds.
         -> Signal p () a          -- ^ Signal representing the sound.
         -> IO ()
outFileHelp f filepath dur sf = 
  let sr          = rate (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
      dat         = map (fromSample . (*0.999)) 
                        (f (toSamples dur sf)) :: [Int32]
                    -- multiply by 0.999 to avoid wraparound at 1.0
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in exportFile filepath aud

{- RealTime Audio -}

-- | Plays a signal to the default speaker
playSignal :: forall a p. (AudioSample a, Clock p) => 
              Double              -- ^ Duration to play in seconds.
           -> Signal p () a       -- ^ Signal representing the sound.
           -> IO ()
playSignal     = playSignalHelp id

-- | Like playSignal, but normalizes the audio stream before playing it.
-- Note: This will compute the entire audio before it starts playing.
playSignalNorm :: forall a p. (AudioSample a, Clock p) => 
                  Double              -- ^ Duration to play in seconds.
               -> Signal p () a       -- ^ Signal representing the sound.
               -> IO ()
playSignalNorm = playSignalHelp normList

playSignalHelp :: forall a p. (AudioSample a, Clock p) => 
                  ([Double] -> [Double]) -- ^ Post-processing function.
               -> Double                 -- ^ Duration to play in seconds.
               -> Signal p () a          -- ^ Signal representing the sound.
               -> IO ()
playSignalHelp f dur sf = bracket (openChannel sr) closeChannel (\c -> mapM_ (writeChannel c) dat)
    where sr    = rate (undefined :: p)
          dat   = f (toSamples dur sf)
          durMS = round (dur * 1000 * 1000)

toSamples :: forall a p. (AudioSample a, Clock p) =>
             Double -> Signal p () a -> [Double]
toSamples dur sf = 
  let sr          = rate     (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
  in take numSamples $ concatMap collapse $ unfold $ strip sf

-- | Compute the maximum sample of an SF in the first 'dur' seconds.
maxSample :: forall a p. (AudioSample a, Clock p) =>
             Double -> Signal p () a -> Double
maxSample dur sf = maximum (map abs (toSamples dur sf))
