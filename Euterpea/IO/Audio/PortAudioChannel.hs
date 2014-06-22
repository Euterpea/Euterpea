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
import Control.Concurrent.STM.TQueue

data PortAudioChannel a = PortAudioChannel (TQueue a) ThreadId

{- Initialization Routines -}
connectedClients :: MVar Integer
connectedClients = unsafePerformIO $ newMVar 0

audioSubsystemInit :: IO ()
audioSubsystemInit = do
    cc <- takeMVar connectedClients
    when (cc == 0) (putStrLn "on" >> void PA.initialize)
    putMVar connectedClients (cc+1)

audioSubsystemShutdown :: IO ()
audioSubsystemShutdown = do
    cc <- takeMVar connectedClients
    when (cc == 1) (putStrLn "off" >> void PA.terminate)
    putMVar connectedClients (cc-1)
    
{- PortAudio Callback -}
paCallback :: forall a. AudioSample a => TQueue a -> PA.StreamCallback CFloat CFloat
paCallback chan _ _ cBufSize _ out = do
    let bufSize = fromIntegral cBufSize
    let nToGet = bufSize `quot` (numChans (undefined :: a))
    samples <- replicateM nToGet (atomically $ readTQueue chan)
    zipWithM_ (pokeElemOff out) [0..(bufSize-1)] (map realToFrac (concatMap collapse samples))
    return PA.Continue

{- Channel Operations -}
openChannel :: forall a. AudioSample a =>
               Double                   -- ^ Signal Rate
            -> IO (PortAudioChannel a)  -- ^ Writable channel
openChannel sr = do
    audioSubsystemInit
    channel <- atomically $ newTQueue
    let playback = Just (paCallback channel)
    let cleanup  = Just (return ())
    let nChan    = numChans (undefined :: a)
    tId <- forkFinally (void $ PA.withDefaultStream 0 nChan sr (Just 512) playback cleanup $ \s ->
                          bracket_ (PA.stopStream s) (PA.stopStream s)
                            (forever (threadDelay (30*1000*1000)) >>= (return . Right)))
                       (\_ -> audioSubsystemShutdown)
    return (PortAudioChannel channel tId)

closeChannel :: forall a. AudioSample a => PortAudioChannel a -> IO ()
closeChannel (PortAudioChannel _ tId) = killThread tId

readChannel :: forall a. AudioSample a => PortAudioChannel a -> IO a
readChannel (PortAudioChannel channel _) = atomically $ readTQueue channel

writeChannel :: forall a. AudioSample a => PortAudioChannel a -> a -> IO ()
writeChannel (PortAudioChannel channel _) = atomically . (writeTQueue channel)
