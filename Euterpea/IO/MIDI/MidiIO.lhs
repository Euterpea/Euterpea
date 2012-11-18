> module Euterpea.IO.MIDI.MidiIO (
>   defaultOutput, defaultInput,
>   playMidi, recordMidi, playTrackRealTime,
>   midiOutRealTime, midiInRealTime, 
>   outputMidi, pollMidi, getOutDev, 
>   makePriorityChannel, tryOutputMidi,
>   terminateMidi,
>   MidiMessage(..),
>   getAllDeviceInfo,
>   printAllDeviceInfo,
>   isValidInputDevice, isValidOutputDevice,
>   getDeviceId,
>   Time,
>   getTimeNow) where

> import Codec.Midi
> import Sound.PortMidi
> import Data.Bits
> import Control.Concurrent
> import Control.Exception
> import System.IO
> import Control.Concurrent.STM.TChan
> import Control.Monad.STM (atomically)

> import Control.Monad
> import Data.IORef
> import Data.List
> import System.IO.Unsafe
> import qualified Data.Heap as Heap

> type MidiEvent = (Time, MidiMessage)

> data MidiMessage = ANote { channel :: !Channel, key :: !Key,
>                           velocity :: !Velocity, duration :: !Time }
>                  | Std Message
>   deriving Show

> getAllDeviceInfo = do
>   n <- countDevices
>   mapM getDeviceInfo [0..n-1]

> printAllDeviceInfo = do
>   devs <- getAllDeviceInfo
>   mapM_ print devs

> getDeviceId isInput n = do
>   devs <- getAllDeviceInfo
>   return $ findIndex (\d -> name d == n && input d == isInput) devs

> isValidDevice pred i = do
>   n <- countDevices   
>   info <- getAllDeviceInfo
>   return $ 
>     i >= 0 && i < n && pred (info !! i)

> isValidInputDevice, isValidOutputDevice :: DeviceID -> IO Bool
> isValidInputDevice = isValidDevice input
> isValidOutputDevice = isValidDevice output

> outDevMap = unsafePerformIO $ 
>   newIORef ([] :: [(DeviceID, 
>                     (PrioChannel Time Message, 
>                      (Time,Message) -> IO (), -- output function
>                      IO () -- stop function
>                   ))])

> data PrioChannel a b = PrioChannel
>     { get           :: IO (Heap.MinPrioHeap a b),
>       push          :: a -> b -> IO (),
>       pop           :: IO (a,b),
>       peek          :: IO (Maybe (a,b)) }

> terminateMidi = do
>   inits <- readIORef outDevMap
>   mapM_ (\(_, (_,out,stop)) -> stop)  inits
>   terminate
>   modifyIORef outDevMap (const [])
>   writeIORef outPort []
>   writeIORef inPort []

> makePriorityChannel = do
>   heapRef <- newIORef (Heap.empty :: Heap.MinPrioHeap Time Message)
>   let get = readIORef heapRef
>       push a b = modifyIORef heapRef (Heap.insert (a,b))
>       pop = do
>         h <- get
>         let (a, h') = Heap.extractHead h
>         modifyIORef heapRef (\_ -> h')
>         return a
>       peek = do
>         h <- get
>         if Heap.isEmpty h 
>           then return Nothing 
>           else return $ Just $ Heap.head h
>         
>   return $ PrioChannel get push pop peek


> getOutDev devId = do
>   inits <- readIORef outDevMap
>   case lookup devId inits of
>     Just f -> return f
>     Nothing -> do
>         x <- midiOutRealTime' devId -- Changes made by Donya Quick: this line used to pattern match against Just.
>         pChan <- makePriorityChannel
>         case x of Just (mout,stop) -> do -- Case statement added.
>         				modifyIORef outDevMap ((devId,(pChan,mout,stop)):)
>         				return (pChan,mout,stop)
>                   Nothing -> return (pChan, const (return ()), return ()) -- Nothing case added



> tryOutputMidi :: DeviceID -> IO ()
> tryOutputMidi devId = do
>   (pChan,out,stop) <- getOutDev devId
>   let loop = do
>         r <- peek pChan
>         case r of
>           Nothing     -> return ()
>           Just (t,m)  -> do
>             now <- getTimeNow
>             if t <= now 
>               then out (now, m) >> pop pChan >> loop
>               else return ()
>   loop
>   return ()

> outputMidi :: DeviceID -> MidiEvent -> IO ()
> outputMidi devId (t,m) = do
>   (pChan,out,stop) <- getOutDev devId
>   now <- getTimeNow
>   let deliver t m = do
>       if t == 0
>         then out (now,m) 
>         else push pChan (now+t) m
>              
>   case m of
>     Std m -> deliver t m
>     ANote c k v d -> do
>         deliver t     (NoteOn c k v)
>         deliver (t+d) (NoteOff c k v)



> pollMidi :: DeviceID -> ((Time,Message) -> IO ()) -> IO ()
> pollMidi devId callback = do
>   s <- lookupPort inPort devId 
>   case s of
>     Nothing -> do
>       r <- openInput devId 
>       case r of
>         Right e -> reportError "pollMIDI" e >> return ()
>         Left s -> addPort inPort devId s >> input s
>     Just s -> input s 
>   where
>     input s = do
>       e <- readEvents s
>       case e of
>         Right e -> if e == NoError 
>           then return () 
>           else reportError "pollMIDI" e >> return ()
>         Left l -> do
>           t <- getTimeNow
>           sendEvts t l
>       where 
>         sendEvts now [] = return () 
>         sendEvts now (e@(PMEvent m t):l) =
>           case msgToMidi m of
>             Just m' -> callback (now, m') >> sendEvts now l
>             Nothing -> sendEvts now l

> defaultOutput :: (DeviceID -> a -> IO b) -> a -> IO b
> defaultOutput f a = do
>   i <- getDefaultOutputDeviceID
>   case i of
>     Nothing -> error "No MIDI output device found"
>     Just i  -> f i a
> 
> defaultInput :: (DeviceID -> a -> IO b) -> a -> IO b
> defaultInput f a = do
>   i <- getDefaultInputDeviceID
>   case i of
>     Nothing -> error "No MIDI input device found"
>     Just i  -> f i a
>  
> playMidi :: DeviceID -> Midi -> IO ()
> playMidi device midi@(Midi _ division _) = do
>   let track = toRealTime division (toAbsTime (head (tracks (toSingleTrack midi))))
>   out <- midiOutRealTime device
>   case out of
>     Nothing -> return ()
>     Just (out, stop) -> do
>       t0 <- getTimeNow 
>       finally (playTrack t0 0 out track) stop
>   where
>     playTrack t0 t' out [] = out (t0 + t', TrackEnd)
>     playTrack t0 t' out (e@(t, m) : s) = do
>       out (t0 + t, m) 
>       if isTrackEnd m 
>         then return ()
>         else playTrack t0 t out s

> playTrackRealTime device track = do
>   out <- midiOutRealTime device
>   case out of
>     Nothing -> return ()
>     Just (out, stop) -> finally (playTrack out track) stop
>   where
>     playTrack out [] = do
>       t <- getTimeNow
>       out (t, TrackEnd)
>     playTrack out (e@(_, m) : s) = do
>       t <- getTimeNow
>       out (t, m) 
>       if isTrackEnd m 
>         then return ()
>         else playTrack out s

>   
> getTimeNow :: IO Time 
> getTimeNow = do
>   t <- time
>   return (fromIntegral t / 1000)

> {-
>     ticksPerBeat = case division of
>       TicksPerBeat n -> n
>       TicksPerSecond mode nticks -> (256 - mode - 128) * nticks `div` 2 
> -}

> outPort = unsafePerformIO (newIORef [])
> inPort = unsafePerformIO (newIORef [])
> lookupPort p i = do
>       l <- readIORef p  
>       return $ lookup i l
> addPort p i s = do
>       l <- readIORef p
>       writeIORef p ((i,s):l)
>
> midiOutRealTime' :: DeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
> midiOutRealTime' i = do
>   s <- openOutput i 1  
>   case s of
>     Right e -> reportError "outputMidi" e >> return Nothing
>     Left s -> do
>       addPort outPort i s
>       return $ Just (process i, finalize i)
>   where
>     process i (t, msg) = do
>       s <- lookupPort outPort i
>       case s of
>         Nothing -> error ("outputMidi: port " ++ show i ++ " is not opened for output")
>         Just s -> do
>           if isTrackEnd msg 
>               then return ()
>               else case midiEvent msg of
>                 Just m  -> writeMsg s t m
>                 Nothing -> return ()
>     writeMsg s t m = do
>               e <- writeShort s (PMEvent m (round (t * 1e3)))
>               case e of
>                 NoError -> return () 
>                 _ -> reportError "outputMidi" e >> return ()
>     finalize i = do
>       s <- lookupPort outPort i
>       maybe (return undefined) close s
>       return ()

> midiOutRealTime :: DeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
> midiOutRealTime i = do
>   s <- openOutput i 1  
>   case s of
>     Right e -> reportError "outputMidi" e >> return Nothing
>     Left s -> do
>       ch <- atomically newTChan 
>       wait <- newEmptyMVar
>       fin <- newEmptyMVar
>       forkIO (pump s ch wait fin)
>       return $ Just (output s ch wait, stop ch fin)
>   where
>     stop ch fin = atomically (unGetTChan ch Nothing) >> takeMVar fin
>     output s ch wait evt@(_, m) = do
>       atomically $ writeTChan ch (Just evt)
>       if isTrackEnd m then takeMVar wait else return ()
>     pump s ch wait fin = loop
>       where
>         loop = do 
>           e <- atomically $ readTChan ch
>           case e of
>             Nothing -> close s >> putMVar fin ()
>             Just (t, msg) -> do
>               now <- getTimeNow
>               if (t > now + 5) 
>                 then atomically (unGetTChan ch e) >> threadDelay 10000 >> loop
>                 else do 
>                   done <- process t msg
>                   if done 
>                     then waitUntil (t + 1)
>                     else loop 
>           where
>             waitUntil t = do
>               now <- getTimeNow
>               if t > now 
>                 then do
>                   threadDelay $ min 10000 (round((t - now) * 1E6)) 
>                   empty <- atomically $ isEmptyTChan ch
>                   if empty 
>                     then waitUntil t
>                     else do
>                       e <- atomically $ readTChan ch
>                       case e of
>                         Nothing -> finishup 
>                         _ -> waitUntil t
>                 else finishup
>             finishup = putMVar wait () >> close s >> putMVar fin ()
>             process t msg = if isTrackEnd msg 
>               then return True 
>               else case midiEvent msg of
>                 Just m  -> writeMsg t m
>                 Nothing -> return False 
>             writeMsg t m = do
>               e <- writeShort s (PMEvent m (round (t * 1e3)))
>               case e of
>                 NoError -> return False 
>                 BufferOverflow -> putStrLn "overflow" >> threadDelay 10000 >> writeMsg t m
>                 _ -> reportError "outputMidi" e >> return True 

> reportError prompt e = do
>   err <- getErrorText e 
>   hPutStrLn stderr $ prompt ++ ": " ++  err

> {-
> runTrack tpb = runTrack' 0 0 120                 -- 120 beat/s is the default tempo
>   where
>     runTrack' t t0 bps ((_, TempoChange tempo) : l) = 
>       let bps' = 1000000 `div` fromIntegral tempo  
>       in runTrack' t t0 bps' l
>     runTrack' t t0 bps ((t1, m) : l) = 
>       let t' = t + 1000 * fromIntegral (t1 - t0) `div` (tpb * bps)
>       in (t', m) : runTrack' t' t1 bps l
>     runTrack' _ _ _ [] = [] 

> playTrack s ch t0 = playTrack' 0
>   where
>     playTrack' t [] = putStrLn "done" >> putMVar ch Nothing >> return (round (t * 1.0E3))
>     playTrack' _ ((t, e):es) = putMVar ch (Just io) >> playTrack' t es 
>       where 
>         io = case midiEvent e of
>           Just m  -> writeShort s (PMEvent m (t0 + round (t * 1.0E3)))
>           Nothing -> return NoError 
> -}
	 
> midiEvent (NoteOff c p v)         = Just $ PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
> midiEvent (NoteOn c p v)          = Just $ PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
> midiEvent (KeyPressure c p pr)    = Just $ PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
> midiEvent (ControlChange c cn cv) = Just $ PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
> midiEvent (ProgramChange c pn)    = Just $ PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
> midiEvent (ChannelPressure c pr)  = Just $ PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
> midiEvent (PitchWheel c pb)       = Just $ PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
>  where (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF)
> midiEvent _ = Nothing 

> recordMidi :: DeviceID -> (Track Time -> IO ()) -> IO ()
> recordMidi device f = do
>   ch <- newChan
>   final <- midiInRealTime device (\e -> writeChan ch e >> return False)
>   case final of 
>     Nothing  -> return () 
>     Just fin -> do
>       track <- getChanContents ch
>       done <- newEmptyMVar 
>       forkIO (f track >> putMVar done ())  
>       putStrLn "Start recording, hit ENTER when you are done."
>       getLine
>       fin 
>       takeMVar done
>       return ()

> midiInRealTime :: DeviceID -> ((Time, Message) -> IO Bool) -> IO (Maybe (IO ()))
> midiInRealTime device callback = do
>   r <- openInput device 
>   case r of
>     Right e -> reportError "midiInRealTime" e >> return Nothing 
>     Left s -> do
>       fin <- newEmptyMVar
>       forkIO (loop Nothing s fin)
>       return (Just (putMVar fin () >> putMVar fin ()))
>   where
>     loop start s fin = do
>       done <- tryTakeMVar fin
>       t <- getTimeNow
>       case done of
>         Just _ -> close s >> callback (t, TrackEnd) >> takeMVar fin >> return ()
>         Nothing -> do
>           e <- readEvents s
>           case e of
>             Right e -> if e == NoError 
>               then threadDelay 1000 >> loop start s fin
>               else do
>                 reportError "midiInRealTime" e 
>                 callback (t, TrackEnd)
>                 return ()
>             Left l -> do
>               t <- getTimeNow
>               sendEvts start t l
>       where 
>         sendEvts start now [] = loop start s fin
>         sendEvts start now (e@(PMEvent m t):l) = do
>           let t0 = maybe t id start
>           case msgToMidi m of
>             Just m' -> do
>               done <- callback (now + fromIntegral (t - t0) / 1E3, m')
>               if done then close s >> return () else sendEvts (Just t0) now l
>             Nothing -> sendEvts (Just t0) now l

> msgToMidi (PMMsg m d1 d2) = 
>   let k = (m .&. 0xF0) `shiftR` 4
>       c = fromIntegral (m .&. 0x0F)
>   in case k of
>     0x8 -> Just $ NoteOff c (fromIntegral d1) (fromIntegral d2)
>     0x9 -> Just $ NoteOn  c (fromIntegral d1) (fromIntegral d2)
>     0xA -> Just $ KeyPressure c (fromIntegral d1) (fromIntegral d2)
>     0xB -> Just $ ControlChange c (fromIntegral d1) (fromIntegral d2)
>     0xC -> Just $ ProgramChange c (fromIntegral d1)
>     0xD -> Just $ ChannelPressure c (fromIntegral d1)
>     0xE -> Just $ PitchWheel c (fromIntegral (d1 + d2 `shiftL` 8))
>     0xF -> Nothing -- SysEx event not handled
>     _   -> Nothing
 
---
