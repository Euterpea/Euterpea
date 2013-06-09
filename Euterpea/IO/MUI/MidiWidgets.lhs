
> {-# LANGUAGE RecursiveDo, Arrows, TupleSections #-}

> module Euterpea.IO.MUI.MidiWidgets where

> import Euterpea.IO.MUI.SOE
> import Euterpea.IO.MUI.UIMonad
> import Euterpea.IO.MUI.UISF
> import Euterpea.IO.MUI.Widget
> import Euterpea.IO.MIDI.MidiIO
> import Control.SF.AuxFunctions (SEvent, DeltaT, constA, (~++),
>                                 eventBuffer, BufferControl, BufferEvent(..))

> import Control.Arrow
> import Control.Monad (when)

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

> midiIn :: UISF DeviceID (SEvent [MidiMessage])
> midiIn = arr Just >>> uisfPipeE f >>> arr (maybe Nothing id) where
>  f dev = do
>   valid <- isValidInputDevice dev
>   m <- if valid then pollMidi dev else return Nothing
>   return $ fmap (\(_t, ms) -> map Std ms) m
 
> midiOut :: UISF (DeviceID, SEvent [MidiMessage]) ()
> midiOut = arr eventizeInput >>> uisfSinkE f >>> constA () where
>   eventizeInput (_, Nothing) = Nothing
>   eventizeInput (dev, Just ms) = Just (dev, ms)
>   f (dev, ms) = do
>       valid <- isValidOutputDevice dev 
>       when valid $ outputMidi dev >>
>                    mapM_ (\m -> deliverMidiEvent dev (0, m)) ms

 
The midiInM widget takes input from multiple devices and combines 
it into a single stream. 

DWC Notes:
Although the streaming input [(DeviceID, Bool)] seems nice as it fits with 
a checkGroup, it seems cleaner to have simply [DeviceID].  The programmer 
can easily apply (map fst . filter snd) to make the conversion.
That said, perhaps checkGroup should just return the list of true values ...

> midiInM' :: UISF [DeviceID] (SEvent [MidiMessage])
> midiInM' = proc bs -> case bs of
>   [] -> returnA -< Nothing
>   d:ds -> do
>       m  <- midiIn   -< d
>       ms <- midiInM' -< ds
>       returnA -< m ~++ ms
>
> midiInM :: UISF [(DeviceID, Bool)] (SEvent [MidiMessage])
> midiInM = arr (map fst . filter snd) >>> midiInM'


A midiOutM widget sends output to multiple MIDI devices by sequencing
the events through a single midiOut. The same messages are sent to 
each device. The midiOutM is designed to be hooked up to a stream like
that from a checkGroup.

DWC Notes:
Is it common to want to send the same messages to many output devices?
Perhaps it is, but this still seems odd.  Also, I apply the same type 
changes here.

> midiOutM' :: UISF ([DeviceID], SEvent [MidiMessage]) ()
> midiOutM' = proc (bs, e) -> case bs of
>   [] -> returnA -< ()
>   d:ds -> do
>       midiOut   -< (d,  e)
>       midiOutM' -< (ds, e)

> midiOutM :: UISF ([(DeviceID, Bool)], SEvent [MidiMessage]) ()
> midiOutM = first (arr (map fst . filter snd)) >>> midiOutM'

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

> midiOutB :: UISF (DeviceID, SEvent [(DeltaT, MidiMessage)]) Bool
> midiOutB = second (arr $ \e -> (fmap AddDataToEnd e, True, 1)) >>> midiOutB'


> midiOutB' :: UISF (DeviceID, BufferControl MidiMessage) Bool
> midiOutB' = proc (devID, bc) -> do
>   (out, b) <- eventBuffer -< bc
>   let extraMsgs = case bc of
>           (Just Clear, _, _) -> Just clearMsgs
>           (Just (SkipAhead _), _, _) -> Just clearMsgs
>           _ -> Nothing
>   midiOut -< (devID, extraMsgs ~++ out)
>   returnA -< b
>  where clearMsgs = map (\c -> Std (ControlChange c 123 0)) [0..15]


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

 
 
----------------------
 | Device Selection | 
----------------------
selectInput and selectOutput are shortcut widgets for producing a set 
of radio buttons corresponding to the available input and output devices 
respectively.  The output is the DeviceID for the chosen device rather 
that just the radio button index as the radio widget would return.

> selectInput, selectOutput :: UISF () DeviceID
> selectInput = selectDev "Input device" input
> selectOutput = selectDev "Output device" output

> selectDev :: String -> (DeviceInfo -> Bool) -> UISF () DeviceID
> selectDev t f = initialIOAction getAllDevices $ \devices ->
>   let devs = filter (\(i,d) -> f d && name d /= "Microsoft MIDI Mapper") devices
>       defaultChoice = if null devs then (-1) else 0
>   in  title t $ proc _ -> do
>       r <- radio (map (name . snd) devs) defaultChoice -< ()
>       returnA -< if r == -1 then r else fst (devs !! r)


The selectInputM and selectOutputM widgets use checkboxes instead of 
radio buttons to allow the user to select multiple inputs and outputs.
These widgets should be used with midiInM and midiOutM respectively.

> selectInputM, selectOutputM :: UISF () [(DeviceID, Bool)]
> selectInputM = selectDevM "Input devices" input
> selectOutputM = selectDevM "Output devices" output

> selectDevM :: String -> (DeviceInfo -> Bool) -> UISF () [(DeviceID, Bool)]
> selectDevM t f = initialIOAction getAllDevices $ \devices ->
>   let devs = filter (\(i,d) -> f d && name d /= "Microsoft MIDI Mapper") devices
>       (devIDs, devNames) = unzip devs
>   in  title t $ proc _ -> do
>       cs <- checkGroup (map name devNames) -< ()
>       returnA -< zip devIDs cs



