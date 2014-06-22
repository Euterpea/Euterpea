{-# LANGUAGE BangPatterns, ExistentialQuantification, 
    ScopedTypeVariables, FlexibleContexts, Arrows #-}
module Euterpea.IO.Audio.PortAudioChannel
    (PortAudioChannel, 
     openChannel, closeChannel,
     readChannel, writeChannel)
    where

import Euterpea.IO.Audio.Types
import qualified Sound.PortAudio as PA

import Control.Concurrent.MonadIO
import Control.Concurrent (forkFinally)
import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Storable

import System.IO.Unsafe
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

data PortAudioChannel a = PortAudioChannel (TBQueue a) ThreadId

{- Initialization Routines -}
connectedClients :: MVar Integer
connectedClients = unsafePerformIO $ newMVar 0

audioSubsystemInit :: IO ()
audioSubsystemInit = do
    cc <- takeMVar connectedClients
    when (cc == 0) (void PA.initialize)
    putMVar connectedClients (cc+1)

audioSubsystemShutdown :: IO ()
audioSubsystemShutdown = do
    cc <- takeMVar connectedClients
    when (cc == 1) (void PA.terminate)
    putMVar connectedClients (cc-1)
    
{- PortAudio Callback -}
paCallback :: forall a. AudioSample a => TBQueue a -> PA.StreamCallback CFloat CFloat
paCallback chan _ _ cBufSize _ out = do
    let bufSize = fromIntegral cBufSize
    let nToGet = bufSize `quot` (numChans (undefined :: a))
    samples <- replicateM nToGet (atomically $ readTBQueue chan)
    zipWithM_ (pokeElemOff out) [0..(bufSize-1)] (map realToFrac (concatMap collapse samples))
    return PA.Continue

{- Channel Operations -}
openChannel :: forall a. AudioSample a =>
               Int                      -- ^ Buffer size
            -> Double                   -- ^ Signal Rate
            -> IO (PortAudioChannel a)  -- ^ Writable channel
openChannel bufSize sr = do
    audioSubsystemInit
    channel <- newTBQueueIO bufSize
    let playback = Just (paCallback channel)
    let cleanup  = Just (return ())
    let nChan    = numChans (undefined :: a)
    tId <- forkFinally (void $ PA.withDefaultStream 0 nChan sr (Just bufSize) playback cleanup $ \s ->
                          bracket_ (PA.startStream s) (PA.stopStream s)
                            (forever (threadDelay (30*1000*1000)) >>= (return . Right)))
                       (const audioSubsystemShutdown)
    return (PortAudioChannel channel tId)

closeChannel :: forall a. AudioSample a => PortAudioChannel a -> IO ()
closeChannel (PortAudioChannel _ tId) = killThread tId

readChannel :: forall a. AudioSample a => PortAudioChannel a -> IO a
readChannel (PortAudioChannel channel _) = atomically $ readTBQueue channel

writeChannel :: forall a. AudioSample a => PortAudioChannel a -> a -> IO ()
writeChannel (PortAudioChannel channel _) = atomically . (writeTBQueue channel)
