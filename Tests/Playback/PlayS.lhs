Special playback functions
Donya Quick
Last modified: 30-Sept-2014

Playing Music and MIDI values with stricter timing and to 
user-specified MIDI devices rather than just the default.

> module PlayS where
> import Euterpea
> import Codec.Midi
> import Sound.PortMidi
> import System.IO.Unsafe (unsafePerformIO)
> import Control.DeepSeq
> import Euterpea.IO.MIDI.MidiIO
> import Control.Monad
> import Control.Concurrent
> import Data.List
> import Control.Exception

CORRECT PLAYBACK TIMING

> playS1 :: (Performable a) => Music a -> IO ()
> playS1 m =  
>     let x = testMidi m 
>     in  x `deepseq` playM x

> playS1Dev :: (Performable a) => DeviceID -> Music a -> IO ()
> playS1Dev dev m =  
>     let x = testMidi m 
>     in  x `deepseq` playM' dev x

> playS2 :: (Performable a, NFData a) => Music a -> IO ()
> playS2 m = m `deepseq`
>     let x = testMidi m 
>     in  x `deepseq` playM x

> playS2Dev :: (Performable a, NFData a) => DeviceID -> Music a -> IO ()
> playS2Dev dev m = m `deepseq`
>     let x = testMidi m 
>     in  x `deepseq` playM' dev x


PLAYING TO DIFFERENT DEVICES

Function to list all available devices:

> devices = do
>   devs <- getAllDevices
>   let devsIn = filter (input.snd) devs
>       devsOut = filter (output.snd) devs
>       f (devid, devname) = "  "++show devid ++ "\t" ++ name devname ++ "\n"
>       strIn = concatMap f devsIn
>       strOut = concatMap f devsOut
>   putStrLn "\nInput devices: " >> putStrLn strIn 
>   putStrLn "Output devices: " >> putStrLn strOut

Original device-specific playback version: 
(fails on long delays, just like "play") 

> playDev :: Performable a => DeviceID -> Music a -> IO ()
> playDev devID m = playM' devID $ testMidi m

Redefinition of playM to support the above.

> playM' :: DeviceID -> Midi -> IO ()
> playM' devID midi = do 
>   initialize
>   playMidi devID midi 
>   terminate
>   return ()

Strict-timing version:

> playDevS1 :: Performable a => DeviceID -> Music a -> IO ()
> playDevS1 devID m = 
>     let x = testMidi m
>     in  x `deepseq` playM' devID x

> playDevS2 :: (Performable a, NFData a) => DeviceID -> Music a -> IO ()
> playDevS2 devID m = m `deepseq`
>     let x = testMidi m
>     in  x `deepseq` playM' devID x


=======================

Infinite playback functions

> playInf, playInfD :: Performable a => Music a -> IO ()
> playInf = playInfDev 0 -- linear channel assignment (error after 15 instruments + percussion)
> playInfD = playInfDevD 0 -- dynamic channel assignment for 16+ instruments

Versions of the above to play to different devices:

> playInfDev, playInfDevD :: Performable a => DeviceID -> Music a -> IO ()
> playInfDev = playInf' 1.0 (linear 16 9)
> playInfDevD = playInf' 1.0 (dynamic 16 9)

Playback to a particular device using a particular channel assignment policy.
The first argument to playInf' is the amount in seconds by which to delay 
closing the MIDI output. Often 1.0sec is enought to avoid clipping with the 
default Windows synth. Other synthesizers with better latency will not need
such a big delay, if any.

> playInf' ::  Performable a => Euterpea.Time -> ChannelMapFun -> DeviceID -> Music a -> IO ()
> playInf' dSec cp devID m = handleCtrlC $ do
>     initializeMidi
>     playRec devID $ musicToMsgs' cp m 
>     threadDelay $ round (dSec * 1000000)
>     terminateMidi
>     return () where
>     handleCtrlC :: IO a -> IO a
>     handleCtrlC op = onException op terminateMidi

> playS' ::  (NFData a, Performable a) => ChannelMapFun -> DeviceID -> Music a -> IO ()
> playS' cp devID m = m `deepseq`
>     let msgs = musicToMsgs' cp m
>     in  msgs `deepseq` do
>         initializeMidi
>         playRec devID msgs 
>         terminateMidi
>         return ()

> playSL, playSD ::  (NFData a, Performable a) => DeviceID -> Music a -> IO ()
> playSL = playS' (linear 16 9)
> playSD = playS' (dynamic 16 9)

> playRec dev [] = return ()
> playRec dev (x@(t,m):ms) = 
>     if t > 0 then threadDelay (toMicroSec t) >> playRec dev ((0,m):ms) else 
>     let mNow = x : takeWhile ((<=0).fst) ms
>         mLater = drop (length mNow - 1) ms
>     in  doMidiOut dev (Just $ mNow) >> playRec dev mLater where
>     doMidiOut dev ms = do
>         valid <- isValidOutputDevice dev 
>         when valid $ outputMidi dev >> maybe (return ()) 
>                      (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms
>     toMicroSec x = round (x * 1000000)
  
==================

Music to Message conversion

Music to message conversion will take place differently depending
on the channel assignment method. linear will assign the first n 
instruments to channels 0 through n-1 (or 1 through n). dynamic 
will fill up n channels and then replace the last-used instrument's
channel with the new instrument.

Some synthesizers only recognize 10 unique channels, others use the
full 16 allowed by general MIDI. Drums are usually on channel 9 
(channel 10 when indexing from 1), but not always.  Sometimes drums
can be assigned to a custom channel.

A ChannelMap stores which instrument is assigned to which channel.

> type ChannelMap = [(InstrumentName, Channel)]

Given an InstrumentName and a ChannelMap, a ChannelMapFun picks a new
channel to assign to the instrument and retruns both that and the 
updated ChannelMap.

> type ChannelMapFun = InstrumentName -> ChannelMap -> (Channel, ChannelMap)

The function below first converts to ANote values and then to Std On/Off 
pairs. This is needed to avoid timing issues associated with using ANote
and trying to call terminateMIDI, since if there is an ANote at the end
it will sometimes have its NoteOff lost, which can cause errors.

> musicToMsgs' :: (Performable a) => 
>     ChannelMapFun -> Music a -> [(Euterpea.Time, MidiMessage)]
> musicToMsgs' cf m = 
>     let (p,dt) = perfDur defPMap defCon m -- obtain the performance 
>         evsA = channelMap cf [] p -- time-stamped ANote values
>         evs = stdMerge evsA -- merged On/Off events sorted by absolute time
>         times = map fst evs -- absolute times in seconds
>         newTimes = zipWith subtract (head times : times) times -- relative times
>     in  zip newTimes (map snd evs)

> stdMerge :: [(Euterpea.Time, MidiMessage)] -> [(Euterpea.Time, MidiMessage)]
> stdMerge [] = []
> stdMerge ((t,ANote c k v d):es) = 
>     (t, Std $ NoteOn c k v) : 
>     stdMerge (insertBy (\(a,b) (x,y) -> compare a x) (t+d, Std $ NoteOff c k v) es) 
> stdMerge (e1:es) = e1 : stdMerge es 

> channelMap :: ChannelMapFun -> ChannelMap -> [Event] -> [(Euterpea.Time, MidiMessage)]
> channelMap cf cMap [] = []
> channelMap cf cMap (e:es) = 
>     let i = eInst e
>         ((chan, cMap'), newI) = case lookup i cMap of Nothing -> (cf i cMap, True)
>                                                       Just x  -> ((x, cMap), False)
>         e' = (fromRational (eTime e), ANote chan (ePitch e) (eVol e) (fromRational $ eDur e))
>         es' = channelMap cf cMap' es
>         iNum = if i==Percussion then 0 else fromEnum i
>     in  if newI then (fst e', Std $ ProgramChange chan iNum) : e' : es' 
>         else e' : es' 

The linear channel map just fills up channels until it hits the maximum
number and then throws an error.

> type NumChannels = Int
> type PercChan = Int

> linear :: NumChannels -> PercChan -> ChannelMapFun 
> linear cLim pChan i cMap = if i==Percussion then (pChan, (i,pChan):cMap) else 
>     let n = length $ filter ((/=Percussion). fst) cMap
>         newChan = if n>=pChan then n+1 else n -- step over percussion channel 
>     in if newChan < cLim then (newChan, (i, newChan) : cMap) else
>        error ("Cannot use more than "++show cLim++" instruments.")  
>     

For the dynamic channel map, new assignements are added in the left side 
of the channel map/list. This means that the item farthest to the right 
is the oldest and should be replaced when the table is full. Percussion
is handled separately.

> dynamic :: NumChannels -> PercChan -> ChannelMapFun 
> dynamic cLim pChan i cMap = 
>     if i==Percussion then (pChan, (i, pChan):cMap) else
>         let cMapNoP = filter ((/=Percussion). fst) cMap
>             extra = if length cMapNoP == length cMap then [] else [(Percussion, pChan)]
>             newChan = snd $ last cMapNoP 
>         in  if length cMapNoP < cLim - 1 then linear cLim pChan i cMap
>         else (newChan, (i, newChan) : (take (length cMapNoP - 1) cMapNoP)++extra)

=======================

NFData instances for Midi

> instance NFData FileType where
>     rnf x = ()

> instance NFData TimeDiv where
>     rnf (TicksPerBeat i) = rnf i
>     rnf (TicksPerSecond i j) = rnf j `seq` rnf i

> instance NFData Midi where
>     rnf (Midi ft td ts) = rnf ft `seq` rnf td `seq` rnf ts

> instance NFData Message where
>     rnf (NoteOff c k v) = rnf c `seq` rnf k `seq` rnf v
>     rnf (NoteOn c k v) = rnf c `seq` rnf k `seq` rnf v
>     rnf (KeyPressure c k v) = rnf c `seq` rnf k `seq` rnf v
>     rnf (ProgramChange c v) = rnf c `seq` rnf v
>     rnf (ChannelPressure c v) = rnf c `seq` rnf v
>     rnf (PitchWheel c v) = rnf c `seq` rnf v
>     rnf (TempoChange t) = rnf t
>     rnf x = () -- no other message types are used by Euterpea

> instance NFData MidiMessage where 
>     rnf (Std m) = rnf m
>     rnf (ANote c k v d) = rnf c `seq` rnf k `seq` rnf v `seq` rnf d

==================

NFData instances for Music

> instance NFData a => NFData (Music a) where
>     rnf (a :+: b) = rnf a `seq` rnf b
>     rnf (a :=: b) = rnf a `seq` rnf b
>     rnf (Prim p) = rnf p
>     rnf (Modify c m) = rnf c `seq` rnf m

> instance NFData a => NFData (Primitive a) where
>     rnf (Note d a) = rnf d `seq` rnf a
>     rnf (Rest d) = rnf d

> instance NFData Control where
>     rnf (Tempo t) = rnf t
>     rnf (Transpose t) = rnf t
>     rnf (Instrument i) = rnf i
>     rnf (Phrase xs) = rnf xs
>     rnf (Player p) = rnf p
>     rnf (KeySig r m) = rnf r `seq` rnf m

> instance NFData PitchClass where
>     rnf p = ()

> instance NFData Mode where
>     rnf x = ()

> instance NFData PhraseAttribute where
>     rnf x = () -- Placeholder; should be filled in later

> instance NFData InstrumentName where
>     rnf x = ()

==================

For dealing with port errors during testing (since terminate, etc. 
may not be in scope if PlayS is imported into another module):

> refresh = terminateMidi
> refresh2 = terminate >> initialize
