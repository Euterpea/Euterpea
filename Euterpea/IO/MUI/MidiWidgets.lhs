> {-# LANGUAGE DoRec, Arrows, TupleSections #-}

> module Euterpea.IO.MUI.MidiWidgets (
>   midiIn
> , midiOut
> , midiInM
> , midiOutMB
> , musicToMsgs
> , musicToBO
> , selectInput,  selectOutput
> , selectInputM, selectOutputM
> , BufferOperation (..) -- Reexported for use with midiOutMB 
> ) where

> import FRP.UISF
> import FRP.UISF.AuxFunctions (liftAIO, initialAIO)
> import Euterpea.IO.MIDI.MidiIO

> import Control.Monad (when, liftM)

> -- These four lines are just for musicToMsgs
> import Euterpea.IO.MIDI.GeneralMidi (toGM)
> import Euterpea.Music.Note.Performance (Music1, Event (..), perform, defPMap, defCon)
> import Euterpea.Music.Note.Music (InstrumentName)
> import Data.List (nub, elemIndex, sortBy)


============================================================
========================= Widgets ==========================
============================================================

-------------------
 | Midi Controls | 
-------------------
midiIn is a widget that accepts a MIDI device ID and returns the event 
stream of MidiMessages that that device is producing.

midiOut is a widget that accepts a MIDI device ID as well as a stream 
of MidiMessages and sends the MidiMessages to the device.

> midiIn :: UISF (Maybe InputDeviceID) (SEvent [MidiMessage])
> midiIn = liftAIO f where
>  f Nothing = return Nothing
>  f (Just dev) = do
>   m <- pollMidi dev
>   return $ fmap (\(_t, ms) -> map Std ms) m
 
> midiOut :: UISF (Maybe OutputDeviceID, SEvent [MidiMessage]) ()
> midiOut = liftAIO f where
>   f (Nothing, _) = return ()
>   f (Just dev, Nothing) = outputMidi dev
>   f (Just dev, Just ms) = do
>       outputMidi dev >> mapM_ (\m -> deliverMidiEvent dev (0, m)) ms

 
The midiInM widget takes input from multiple devices and combines 
it into a single stream. 

> midiInM :: UISF [InputDeviceID] (SEvent [MidiMessage])
> midiInM = foldA (~++) Nothing (arr Just >>> midiIn)

> midiInM' :: UISF [(InputDeviceID, Bool)] (SEvent [MidiMessage])
> midiInM' = arr (map fst . filter snd) >>> midiInM


A midiOutM widget sends output to multiple MIDI devices by sequencing
the events through a single midiOut. The same messages are sent to 
each device. The midiOutM is designed to be hooked up to a stream like
that from a checkGroup.

> midiOutM :: UISF [(OutputDeviceID, SEvent [MidiMessage])] ()
> midiOutM = foldA const () (arr (first Just) >>> midiOut)

> midiOutM' :: UISF ([(OutputDeviceID, Bool)], SEvent [MidiMessage]) ()
> midiOutM' = arr fixData >>> midiOutM where
>   fixData (lst, mmsgs) = map ((,mmsgs) . fst) $ filter snd lst


A midiOutB widget wraps the regular midiOut widget with a buffer. 
This allows for a timed series of messages to be prepared and sent
to the widget at one time. With the regular midiOut, there is no
timestamping of the messages and they are assumed to be played "now"
rather than at some point in the future. Just as MIDI files have the
events timed based on ticks since the last event, the events here 
are timed based on seconds since the last event. If an event is 
to occur 0.0 seconds after the last event, then it is assumed to be
played at the same time as that other event and all simultaneous 
events are handed to midiOut at the same timestep. Finally, the 
widget returns a flat that is True if the buffer is empty and False
if the buffer is full (meaning that items are still being played).

> midiOutB :: UISF (Maybe OutputDeviceID, BufferOperation MidiMessage) Bool
> midiOutB = proc (devID, bo) -> do
>   (out, b) <- eventBuffer -< bo
>   midiOut -< (devID, if shouldClear bo then Just clearMsgs ~++ out else out)
>   returnA -< b
>  where clearMsgs = map (\c -> Std (ControlChange c 123 0)) [0..15]
>        shouldClear ClearBuffer = True
>        shouldClear (SkipAheadInBuffer _) = True
>        shouldClear (SetBufferPlayStatus _ bo) = shouldClear bo
>        shouldClear (SetBufferTempo      _ bo) = shouldClear bo
>        shouldClear _ = False

> midiOutB' :: UISF (Maybe OutputDeviceID, SEvent [(DeltaT, MidiMessage)]) Bool
> midiOutB' = second (arr $ maybe NoBOp AppendToBuffer) >>> midiOutB


The midiOutMB widget combines the power of midiOutM with midiOutB, allowing 
multiple sets of buffer controlled midi messages to be sent to different 
devices.  The Bool output is True if every buffer is empty (that is, no device 
has any pending music to be played) and False otherwise.

> midiOutMB :: UISF [(OutputDeviceID, BufferOperation MidiMessage)] Bool
> midiOutMB = foldA (&&) True (arr (first Just) >>> midiOutB)

> midiOutMB' :: UISF ([(OutputDeviceID, Bool)], SEvent [(DeltaT, MidiMessage)]) Bool
> midiOutMB' = arr fixData >>> midiOutMB where
>   fixData (lst, mmsgs) = map ((,maybe NoBOp AppendToBuffer mmsgs) . fst) $ filter snd lst


The musicToMsgs function bridges the gap between a Music1 value and
the input type of midiOutB. It turns a Music1 value into a series 
of MidiMessages that are timestamped using the number of seconds 
since the last event. The arguments are as follows:

- True if allowing for an infinite music value, False if the input
  value is known to be finite. 

- InstrumentName overrides for channels for infinite case. When the
  input is finite, an empty list can be supplied since the instruments
  will be pulled from the Music1 value directly (which is obviously 
  not possible to do in the infinite case).

- The Music1 value to convert to timestamped MIDI messages.

> musicToMsgs :: Bool -> [InstrumentName] -> Music1 -> [(DeltaT, MidiMessage)]
> musicToMsgs inf is m = 
>     let p = perform defPMap defCon m -- obtain the performance
>         instrs = if null is && not inf then nub $ map eInst p else is
>         chan e = 1 + case elemIndex (eInst e) instrs of 
>                          Just i -> i
>                          Nothing -> error ("Instrument "++show (eInst e)++
>                                     "is not assigned to a channel.")                               
>         f e = (eTime e, ANote (chan e) (ePitch e) (eVol e) (fromRational $ eDur e))
>         f2 e = [(eTime e, Std (NoteOn (chan e) (ePitch e) (eVol e))), 
>                (eTime e + eDur e, Std (NoteOff (chan e) (ePitch e) (eVol e)))]
>         evs = if inf then map f p else sortBy mOrder $ concatMap f2 p -- convert to MidiMessages
>         times = map (fromRational.fst) evs -- absolute times
>         newTimes = zipWith subtract (head times : times) times -- relative times
>         progChanges = zipWith (\c i -> (0, Std $ ProgramChange c i)) 
>                       [1..16] $ map toGM instrs
>     in  if length instrs > 16 then error "too many instruments!" 
>         else progChanges ++ zip newTimes (map snd evs) where
>     mOrder (t1,m1) (t2,m2) = compare t1 t2

> musicToBO :: Bool -> [InstrumentName] -> Music1 -> BufferOperation MidiMessage
> musicToBO inf is m = AppendToBuffer $ musicToMsgs inf is m
 
 
----------------------
 | Device Selection | 
----------------------
selectInput and selectOutput are shortcut widgets for producing a set 
of radio buttons corresponding to the available input and output devices 
respectively.  The output is the DeviceID for the chosen device rather 
that just the radio button index as the radio widget would return.

> selectInput  :: UISF () (Maybe InputDeviceID)
> selectOutput :: UISF () (Maybe OutputDeviceID)
> selectInput  = selectDev "Input device"  (liftM fst $ getAllDevices)
> selectOutput = selectDev "Output device" (liftM snd $ getAllDevices)

> selectDev :: String -> IO [(deviceid, DeviceInfo)] -> UISF () (Maybe deviceid)
> selectDev t getDevs = initialAIO getDevs $ \devices ->
>   let devs = filter (\(i,d) -> name d /= "Microsoft MIDI Mapper") devices
>       defaultChoice = if null devs then (-1) else 0
>   in  title t $ proc _ -> do
>       r <- radio (map (name . snd) devs) defaultChoice -< ()
>       returnA -< if r == -1 then Nothing else Just $ fst (devs !! r)


The selectInputM and selectOutputM widgets use checkboxes instead of 
radio buttons to allow the user to select multiple inputs and outputs.
These widgets should be used with midiInM and midiOutM respectively.

> selectInputM  :: UISF () [InputDeviceID]
> selectOutputM :: UISF () [OutputDeviceID]
> selectInputM  = selectDevM "Input devices"  (liftM fst $ getAllDevices)
> selectOutputM = selectDevM "Output devices" (liftM snd $ getAllDevices)

> selectDevM :: String -> IO [(deviceid, DeviceInfo)] -> UISF () [deviceid]
> selectDevM t getDevs = initialAIO getDevs $ \devices ->
>   let devs = filter (\(i,d) -> name d /= "Microsoft MIDI Mapper") devices
>   in  title t $ checkGroup $ map (\(i,d) -> (name d, i)) devs



