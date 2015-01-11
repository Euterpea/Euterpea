
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Euterpea.IO.MIDI.MidiIO (
>   getAllDevices, --isValidInputDevice, isValidOutputDevice, -- Used only by Euterpea.IO.MUI.MidiWidgets
>   terminateMidi, initializeMidi, -- Used only by Euterpea.IO.MUI
>   outputMidi, deliverMidiEvent, -- Used only by Euterpea.IO.MUI.MidiWidgets (particularly by midiOut)
>   pollMidi, -- Used only by Euterpea.IO.MUI.MidiWidgets (particularly by midiIn)
>   defaultOutput, defaultInput,
>   playMidi, 
>   MidiMessage (ANote, Std), 
>   getTimeNow,
>   DeviceInfo(..), InputDeviceID, OutputDeviceID, Message(..), Time,
>   unsafeInputID, unsafeOutputID,
> ) where

> import Codec.Midi (Time, Channel, Key, Velocity, 
>                    Message (..), Midi (..), Track, 
>                    toRealTime, toAbsTime, toSingleTrack, isTrackEnd)
> import Sound.PortMidi (DeviceInfo (..), getDeviceInfo, 
>                        DeviceID, countDevices, time, 
>                        getDefaultOutputDeviceID, getDefaultInputDeviceID, 
>                        openInput, openOutput, readEvents, 
>                        close, writeShort, getErrorText, terminate, initialize, 
>                        PMError (NoError, BufferOverflow), PMStream, 
>                        PMEvent (..), PMMsg (PMMsg))
> import Control.Exception (finally)
> import Control.Concurrent
> import Control.Concurrent.STM.TChan
> import Control.Monad.STM (atomically)
> import Data.IORef

> import Data.Bits (shiftR, shiftL, (.|.), (.&.))
> import Data.List (findIndex)
> import Data.Maybe (mapMaybe)
> import qualified Data.Heap as Heap

> import System.IO (hPutStrLn, stderr)
> import System.IO.Unsafe (unsafePerformIO)
> import Control.DeepSeq (NFData)


----------------------------
 | Midi Type declarations | 
----------------------------

> type MidiEvent = (Time, MidiMessage)

> data MidiMessage = ANote { channel :: !Channel, key :: !Key,
>                           velocity :: !Velocity, duration :: !Time }
>                  | Std Message
>   deriving Show

> newtype InputDeviceID  = InputDeviceID  DeviceID
>   deriving (Eq, Show, NFData)
> newtype OutputDeviceID = OutputDeviceID DeviceID
>   deriving (Eq, Show, NFData)

> unsafeInputID :: Int -> InputDeviceID
> unsafeInputID = InputDeviceID

> unsafeOutputID :: Int -> OutputDeviceID
> unsafeOutputID = OutputDeviceID

----------
 | Time | 
----------

Is this the time we want?  This comes from PortMidi, but there's also the 
function FRP.UISF.SOE.timeGetTime which uses time data from GLFW.

> getTimeNow :: IO Time 
> getTimeNow = do
>   t <- time
>   return (fromIntegral t / 1000)


----------------------
 | Device Functions | 
----------------------

getAllDevices returns a list of all of the DeviceInfos found.
It calls Port.Midi.getDeviceInfo over all device numbers

> getAllDevices :: IO ([(InputDeviceID, DeviceInfo)], [(OutputDeviceID, DeviceInfo)])
> getAllDevices = do
>   n <- countDevices
>   deviceInfos <- mapM getDeviceInfo [0..n-1]
>   let devs = zip [0..n-1] deviceInfos
>   return ([ (InputDeviceID  d, i) | (d,i) <- devs, input  i], 
>           [ (OutputDeviceID d, i) | (d,i) <- devs, output i])


isValidInputDevice and isValideOutputDevice check whether the given 
devices are respectively valid for input or output.

isValidInputDevice, isValidOutputDevice :: DeviceID -> IO Bool
isValidInputDevice = isValidDevice input
isValidOutputDevice = isValidDevice output
isValidDevice :: (DeviceInfo -> Bool) -> DeviceID -> IO Bool
isValidDevice pred i = do
  n <- countDevices   
  info <- getAllDevices
  return $ 
    i >= 0 && i < n && pred (snd $ info !! i)


---------------------
 | Default devices | 
---------------------

Rather than export the deviceIDs directly, these two functions allow 
the caller to use the DeviceID without directly controlling it.

They take a function (such as playMidi) and an auxiary argument and 
apply them together with the default device.  If no default device 
exists, an error is thrown.

> defaultOutput :: (OutputDeviceID -> a -> IO b) -> a -> IO b
> defaultOutput f a = do
>   i <- getDefaultOutputDeviceID
>   case i of
>     Nothing -> error "No MIDI output device found"
>     Just i  -> f (OutputDeviceID i) a
> 
> defaultInput :: (InputDeviceID -> a -> IO b) -> a -> IO b
> defaultInput f a = do
>   i <- getDefaultInputDeviceID
>   case i of
>     Nothing -> error "No MIDI input device found"
>     Just i  -> f (InputDeviceID i) a


-----------------------
 | Priority Channels | 
-----------------------

The priority channel data type and a constructor for it will be used 
by devices.  We define them here.

> data PrioChannel a b = PrioChannel
>     { get           :: IO (Heap.MinPrioHeap a b),
>       push          :: a -> b -> IO (),
>       pop           :: IO (a,b),
>       peek          :: IO (Maybe (a,b)) }

> makePriorityChannel :: IO (PrioChannel Time Message)
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


------------------------
 | Global Device Data | 
------------------------

We keep a mapping from DeviceID to the priority channel for keeping
track of future MIDI messages, an output function to produce sound, 
and a stop function.  This mapping is stored in the global ref 
outDevMap, and it is accessed by getOutDev (which looks up info 
and adds associations if necessary) and terminateMidi (which calls 
the stop function on all elements and clears the mapping).

outDevMap is the global mapping.

> outDevMap :: IORef [(OutputDeviceID, 
>                      (PrioChannel Time Message, -- priority channel
>                       (Time, Message) -> IO (), -- sound output function
>                       IO ()))]                  -- stop/terminate function
> outDevMap = unsafePerformIO $ newIORef []


outPort and inPort are global memory refs that contain a mapping of 
DeviceID to Port Midi Streams.  They are modified with addPort (which 
adds a new mapping to the list) and lookupPort (which, given a DeviceID, 
returns the Port Midi Stream associated with it).

> outPort :: IORef [(OutputDeviceID, PMStream)]
> inPort  :: IORef [(InputDeviceID,  PMStream)]
> outPort = unsafePerformIO (newIORef [])
> inPort  = unsafePerformIO (newIORef [])

> lookupPort :: (Eq deviceid) => IORef [(deviceid, PMStream)] -> deviceid -> IO (Maybe PMStream)
> lookupPort p i = readIORef p >>= (return . lookup i)

> addPort :: IORef [(deviceid, PMStream)] -> (deviceid, PMStream) -> IO ()
> addPort p is = modifyIORef p (is:)


--------------------------------------------------
 | Global Device Initialization and Termination | 
--------------------------------------------------

initializeMidi just initializes PortMidi

> initializeMidi :: IO ()
> initializeMidi = do
>   e <- initialize
>   if e == NoError 
>       then return () 
>       else reportError "initializeMidi" e

terminateMidi calls the stop function on all elements of outDevMap 
and clears the mapping entirely.  It also clears outPort and inPort.

> terminateMidi :: IO ()
> terminateMidi = do
>   inits <- readIORef outDevMap
>   mapM_ (\(_, (_,_out,stop)) -> stop) inits
>   terminate
>   modifyIORef outDevMap (const [])
>   writeIORef outPort []
>   writeIORef inPort []


-------------------
 | Device Lookup | 
-------------------

getOutDev looks up info in outDevMap and adds associations if necessary.  
It is accessed as a helper function for outputMidi and deliverMidiEvent.

> getOutDev :: OutputDeviceID -> IO (PrioChannel Time Message, (Time, Message) -> IO (), IO ())
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


----------------
 | Midi Input | 
----------------

pollMidi take an input device and a callback function and polls the device 
for midi events.  Any events are sent, along with the current time, to 
the callback function.
DWC NOTE: Why is the time even used?  All messages get the same time?

> pollMidiCB :: InputDeviceID -> ((Time, [Message]) -> IO ()) -> IO ()
> pollMidiCB idid@(InputDeviceID devId) callback = do
>   s <- lookupPort inPort idid 
>   case s of
>     Nothing -> do
>       r <- openInput devId 
>       case r of
>         Right e -> reportError "pollMidiCB" e
>         Left s -> addPort inPort (idid, s) >> input s
>     Just s -> input s 
>   where
>     input :: PMStream -> IO ()
>     input s = do
>       e <- readEvents s
>       case e of
>         Right e -> if e == NoError 
>           then return () 
>           else reportError "pollMidiCB" e
>         Left l -> do
>           now <- getTimeNow
>           case mapMaybe (msgToMidi . message) l of
>             [] -> return ()
>             ms -> callback (now, ms)

> pollMidi :: InputDeviceID -> IO (Maybe (Time, [Message]))
> pollMidi idid@(InputDeviceID devId) = do
>   s <- lookupPort inPort idid 
>   case s of
>     Nothing -> do
>       r <- openInput devId 
>       case r of
>         Right e -> reportError "pollMIDI" e >> return Nothing
>         Left s -> addPort inPort (idid, s) >> input s
>     Just s -> input s 
>   where
>     input :: PMStream -> IO (Maybe (Time, [Message]))
>     input s = do
>       e <- readEvents s
>       case e of
>         Right e -> if e == NoError 
>           then return Nothing
>           else reportError "pollMIDI" e >> return Nothing
>         Left l -> do
>           now <- getTimeNow
>           case mapMaybe (msgToMidi . message) l of
>             [] -> return Nothing
>             ms -> return $ Just (now, ms)


---------------------------------------------
 | Midi Output for inidividual Midi events | 
---------------------------------------------

The following two functions are for sending and playing individual 
Midi events to devices.  Typically, usage will be to call outputMidi 
to play anything that's ready to play and then send in the latest 
messages with deliverMidiEvent.  Of course, if no new messages are 
ready to be delivered, that step can be omitted.  Either way, 
outputMidi should be called many times per second to assure that 
all Midi messages are played approximately when scheduled.

deliverMidiEvent sends the given MidiEvent to the given device.  If 
the event is scheduled to happen ``now'', then it is immediately 
played.  Otherwise, it is queued for later.

> deliverMidiEvent :: OutputDeviceID -> MidiEvent -> IO ()
> deliverMidiEvent devId (t,m) = do
>   (pChan, out, _stop) <- getOutDev devId
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


outputMidi plays all midi events that are waiting in this device's 
priority queue whose time to play has come.

> outputMidi :: OutputDeviceID -> IO ()
> outputMidi devId = do
>   (pChan, out, _stop) <- getOutDev devId
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


-------------------------------------------
 | Midi Output for a complete Midi track | 
-------------------------------------------

When an entire Midi track is ready to be played, the playMidi function 
may be more appropriate than deliverMidiEvent and outputMidi.

playMidi will queue up the entire Midi track given to it and then close 
the output device.

> playMidi :: OutputDeviceID -> Midi -> IO ()
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


---------------------
 | midiOutRealTime | 
---------------------

The following two functions are used to open a device for Midi output.  
They should only be called when the device hasn't yet been opened, and 
they both return a ``play'' function and a ``stop'' function.

Currently, midiOutRealTime' is used for Midi output for inidividual 
Midi events, and midiOutRealTime is used for Midi output for a complete 
Midi track.

DWC Notes:
I'm not entirely sure how they both work yet.  midiOutRealTime' 
actually looks pretty straightforward in that it just creates the process 
and stop functions and adds this device to the outPort device list.  The 
process function will look up the device in the outPort device list, and 
if it finds it, it writes the message to it.  The stop function removes 
the device from the outPort list and closes it.

On the other hand, midiOutRealTime spawns a new thread and does some 
concurrent stuff.  Really, it looks similar, but I don't know when to 
use one and when to use the other.

> midiOutRealTime' :: OutputDeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
> midiOutRealTime' odid@(OutputDeviceID devId) = do
>   s <- openOutput devId 1  
>   case s of
>     Right e -> reportError "Unable to open output device in midiOutRealTime'" e >> return Nothing
>     Left s -> do
>       addPort outPort (odid, s)
>       return $ Just (process odid, finalize odid)
>   where
>     process odid (t, msg) = do
>       s <- lookupPort outPort odid
>       case s of
>         Nothing -> error ("midiOutRealTime': port " ++ show odid ++ " is not open for output")
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
>                 _ -> reportError "midiOutRealTime'" e
>     finalize odid = do
>       s <- lookupPort outPort odid
>       e <- maybe (return NoError) close s
>       case e of
>         NoError -> return () 
>         _ -> reportError "midiOutRealTime'" e


> midiOutRealTime :: OutputDeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
> midiOutRealTime odid@(OutputDeviceID devId) = do
>   s <- openOutput devId 1  
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


---------------------
 | MIDI Conversion | 
---------------------

A conversion function from Codec.Midi Messages to PortMidi PMMsgs.

> midiEvent :: Message -> Maybe PMMsg
> midiEvent (NoteOff c p v)         = Just $ PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
> midiEvent (NoteOn c p v)          = Just $ PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
> midiEvent (KeyPressure c p pr)    = Just $ PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
> midiEvent (ControlChange c cn cv) = Just $ PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
> midiEvent (ProgramChange c pn)    = Just $ PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
> midiEvent (ChannelPressure c pr)  = Just $ PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
> midiEvent (PitchWheel c pb)       = Just $ PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
>  where (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF)
> midiEvent _ = Nothing 


A conversion function from PortMidi PMMsgs to Codec.Midi Messages.

> msgToMidi :: PMMsg -> Maybe Message
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


---------------------
 | Error Reporting | 
---------------------

> reportError :: String -> PMError -> IO ()
> reportError prompt e = do
>   err <- getErrorText e 
>   hPutStrLn stderr $ prompt ++ ": " ++  err





----------------------
 | Unused Functions | 
----------------------

> -- Prints all DeviceInfo found by getAllDevices.
> printAllDeviceInfo :: IO ()
> printAllDeviceInfo = do
>   (indevs, outdevs) <- getAllDevices
>   mapM_ (print . snd) indevs
>   mapM_ (print . snd) outdevs

-- Given whether the device is an input device and the device name, 
-- returns the DeviceID.
getDeviceId :: Bool -> String -> IO (Maybe DeviceID)
getDeviceId isInput n = do
  devs <- getAllDevices
  return $ findIndex (\(_,d) -> name d == n && input d == isInput) devs

> playTrackRealTime :: OutputDeviceID -> [(t, Message)] -> IO ()
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

> {-
>     ticksPerBeat = case division of
>       TicksPerBeat n -> n
>       TicksPerSecond mode nticks -> (256 - mode - 128) * nticks `div` 2 
> -}

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
>
> playTrack s ch t0 = playTrack' 0
>   where
>     playTrack' t [] = putStrLn "done" >> putMVar ch Nothing >> return (round (t * 1.0E3))
>     playTrack' _ ((t, e):es) = putMVar ch (Just io) >> playTrack' t es 
>       where 
>         io = case midiEvent e of
>           Just m  -> writeShort s (PMEvent m (t0 + round (t * 1.0E3)))
>           Nothing -> return NoError 
> -}

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

