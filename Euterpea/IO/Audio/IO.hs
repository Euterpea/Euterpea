{-# LANGUAGE BangPatterns, ExistentialQuantification, 
    ScopedTypeVariables, FlexibleContexts, Arrows #-}

module Euterpea.IO.Audio.IO (
    outFile,  outFileNorm, 
    playSignal, playSignalNorm,
--    outFileA, outFileNormA, RecordStatus, 
    maxSample) where

import Prelude hiding (init)
import Control.CCA.Types
import Control.CCA.ArrowP
import Control.Arrow

import Control.SF.SF
import Control.SF.MSF
import Euterpea.IO.MUI.UIMonad
import Euterpea.IO.MUI.UISF
import Control.Concurrent.MonadIO
import Control.Concurrent (forkOS, forkFinally)

import Euterpea.IO.Audio.Types hiding (Signal)

import Codec.Wav
import Data.Audio
import Data.Array.Unboxed
import Data.Int
import Data.IORef
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import qualified Sound.PortAudio.Base as PAB
import qualified Sound.PortAudio as PA

import Control.Monad

import Foreign.C.Types
import Foreign.ForeignPtr

import Control.Exception as E
import System.IO

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
         -> String              -- ^ Filename to write to.
         -> Double              -- ^ Duration of the wav in seconds.
         -> Signal p () a       -- ^ Signal representing the sound.
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

playSignal :: forall a p. (AudioSample a, Clock p) => 
              Double              -- ^ Duration to play in seconds.
           -> Signal p () a       -- ^ Signal representing the sound.
           -> IO ()
playSignal     = playSignalHelp id

playSignalNorm :: forall a p. (AudioSample a, Clock p) => 
                  Double              -- ^ Duration to play in seconds.
               -> Signal p () a       -- ^ Signal representing the sound.
               -> IO ()
playSignalNorm = playSignalHelp normList

paCallback :: MVar [Double] -> PA.StreamCallback CFloat CFloat
paCallback mvar _ _ nSamples _ out = do
    samples <- readMVar mvar
    let (playedS, bufferedS) = splitAt (fromIntegral nSamples) samples
    zipWithM_ (pokeElemOff out) [0..] (map realToFrac playedS)
    swapMVar mvar bufferedS
    return (if null bufferedS then PA.Abort else PA.Continue)

playSignalHelp :: forall a p. (AudioSample a, Clock p) => 
                  ([Double] -> [Double]) -- ^ Post-processing function.
               -> Double              -- ^ Duration to play in seconds.
               -> Signal p () a       -- ^ Signal representing the sound.
               -> IO ()
playSignalHelp f dur sf = 
  let sr    = rate (undefined :: p)
      nChan = numChans (undefined :: a)
      dat   = f (toSamples dur sf)
  in do 
      PA.initialize
      mSamples <- newMVar dat
      -- Set up callbacks
      let playback = Just (paCallback mSamples)
      let cleanup  = Just (return ())
      let dur_uS   = truncate (dur * 1000 * 1000)
      -- Below 512 = size of buffer
      m   <- newEmptyMVar
      tId <- forkFinally (void $ PA.withDefaultStream 0 nChan sr (Just 512) playback cleanup $ \s ->
                  bracket_ (PA.startStream s) (PA.stopStream s)
                    (threadDelay dur_uS) >>= (return . Right))
                (\e -> PA.terminate >> putMVar m e)
      ex  <- try (void $ readMVar m)
      case ex of
        Right () -> return ()
        Left (_ :: SomeException) -> do
          killThread tId

{-
data RecordStatus = Pause | Record | Clear | Write

outFileA :: forall a. AudioSample a => 
            String               -- ^ Filename to write to.
         -> Double               -- ^ Sample rate of the incoming signal.
         -> UISF (a, RecordStatus) ()
outFileA = outFileHelpA id

outFileNormA :: forall a. AudioSample a => 
                String               -- ^ Filename to write to.
             -> Double               -- ^ Sample rate of the incoming signal.
             -> UISF (a, RecordStatus) ()
outFileNormA = outFileHelpA normList

outFileHelpA :: forall a. AudioSample a => 
             ([Double] -> [Double]) -- ^ Post-processing function.
          -> String                 -- ^ Filename to write to.
          -> Double                 -- ^ Sample rate of the incoming signal.
          -> UISF (a, RecordStatus) ()
outFileHelpA f filepath sr = 
  let numChannels = numChans (undefined :: a)
      writeWavSink = sink (writeWav f filepath sr numChannels)
  in proc (a, rs) -> do
        rec dat <- init [] -< dat'
            dat' <- case rs of
                        Pause  -> returnA -< dat
                        Record -> returnA -< a:dat
                        Clear  -> returnA -< []
                        Write  -> do writeWavSink -< dat
                                     returnA -< a:dat
        returnA -< ()
-}

writeWav :: AudioSample a => ([Double] -> [Double]) -> String -> Double -> Int -> [a] -> UI ()
writeWav f filepath sr numChannels adat = 
  let dat         = map (fromSample . (*0.999)) 
                        (f (concatMap collapse adat)) :: [Int32]
                    -- multiply by 0.999 to avoid wraparound at 1.0
      array       = listArray (0, (length dat)-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in liftIO $ exportFile filepath aud



  

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


{-
chunk !nFrames !(i, f) ref buf = nFrames `seq` i `seq` f `seq` aux nFrames i 
    where aux !n !i = x `seq` i `seq` i' `seq`
                       if n == 0 then do
                                  writeIORef ref i
                                  return ()
                       else do
                        pokeElemOff buf (fromIntegral nFrames-n) (realToFrac x)
                        aux (n-1) i'
              where (x, i') = f ((), i)
{-# INLINE [0] chunk #-}

chunkify !i !f !secs = do
  --userData <- new i
  ref <- newIORef i
  let cb :: RtAudioCallback 
      cb oBuf iBuf nFrames nSecs status userData = do
                      
                      lastState <- readIORef ref
                      -- Fill output buffer with nFrames of samples
                      chunk (fromIntegral nFrames) (lastState,f) ref oBuf
                      if secs < (realToFrac nSecs) then return 2 else return 0
                              
                                                          
  mkAudioCallback cb                                 



playPure :: Show b => Double -> (b, ((), b) -> (Double, b)) -> IO ()
playPure !secs !(i, f) = do
  rtaCloseStream
  rtaInitialize
  dev <- rtaGetDefaultOutputDevice
  callback <- chunkify i f secs
  with (StreamParameters dev 1 0) (\params -> do
         rtaOpenStream params nullPtr float64 44100 4096 callback nullPtr nullPtr)
  rtaStartStream
  return ()
  
-}
