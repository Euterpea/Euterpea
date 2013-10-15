> module Euterpea.IO.MIDI.FromMidi (fromMidi) where
> import Euterpea.Music.Note.Music
> import Euterpea.Music.Note.MoreMusic
> import Euterpea.Music.Note.Performance
> import Euterpea.IO.MIDI.ToMidi
> import Euterpea.IO.MIDI.GeneralMidi
> import Data.List
> import Codec.Midi


Donya Quick
Last updated 15-Oct-2013.

Changes since last major version (15-Jan-2013):
- makeUPM: (is !! i, 10) changed to (is !! i, 9) for Percussion.
- Instrument numbers <0 are interpreted as Percussion.
- ProgChange 10 x is now assigned (-1) as an instrument number.

KNOWN ISSUES:
- Tempo changes occuring between matching note on/off events may not be 
  interpreted optimally. A performance-correct representation rather 
  than a score-correct representation could be accomplished by looking 
  for these sorts of between-on-off tempo changes when calculating a 
  note's duration. 
  
This code was originally developed for research purposes and then 
adapted for CPSC 431/531 to overcome some problems exhibited by the
original implementation of fromMidi. 

This code has functions to read Midi values into an intermediate type,
SimpleMsg, before conversion to Music (Pitch, Volume) to make processing 
instrument changes easier. The following features will be retained from 
the input file:
- Placement of notes relative to the beat (assumed to be quarternotes).
- The pitch, volume, and instrument of each note.
- Tempo changes indicated by TempoChange MIDI events

Other MIDI controller information is currently not supported. This includes 
events such as pitch bends and modulations. For these controllers, there is 
no simple way to capture the information in a Music data structure.

The following datatype is for a simplification of MIDI events into simple 
On/off events for pitches occurring at different times. There are two 
types of events considered: tempo changes and note events. The note events
are represented by tuples of:
- exact onset time, Rational
- absolute pitch, AbsPitch
- volume from 0-127, Volume
- instrument number, Int. The value (-1) is used for Percussion.
- on/off type, NEvent

> data NEvent = On | Off
>   deriving (Eq, Show, Ord)

> data SimpleMsg = SE (Rational, AbsPitch, Volume, Int, NEvent) |
>               T (Rational, Rational)
>   deriving (Eq, Show)
> instance Ord (SimpleMsg) where
>     compare (SE(t,p,v,i,e)) (SE(t',p',v',i',e')) = 
>         if t<t' then LT else if t>t' then GT else EQ
>     compare (T(t,x)) (SE(t',p',v',i',e')) = 
>         if t<t' then LT else if t>t' then GT else EQ
>     compare (SE(t,p,v,i,e)) (T(t',x)) = 
>         if t<t' then LT else if t>t' then GT else EQ
>     compare (T(t,x)) (T(t',x')) =
>         if t<t' then LT else if t>t' then GT else EQ

The importFile function places track ticks (Ticks) in a format where 
each value attached to a message represents the number of ticks that 
have passed SINCE THE LAST MESSAGE. The following function will convert 
input in that format into a list of pairs where the ticks are absolute. 
In otherwords, ticks in the output will represent the exact point in 
time of an event. This means that unsupported events (e.g. pitch bend) 
can later be filtered out without affecting the timing of support events.

> addTrackTicks :: Int -> [(Ticks, a)] -> [(Ticks, a)]
> addTrackTicks sum [] = []
> addTrackTicks sum ((t,x):ts) = (t+sum,x) : addTrackTicks (t+sum) ts

The following function addresses a ticks to Music duration conversion.

> applyTD :: TimeDiv -> SimpleMsg -> SimpleMsg
> applyTD tdw x = 
>     case x of T(t,i) -> T(fixT tdw t, i) 
>               SE(t,p,v,i,e) -> SE(fixT tdw t, p, v, i, e) where

> fixT tdw t = 
>     case tdw of TicksPerBeat td -> t / (fromIntegral td * 4)
>                 TicksPerSecond fps tpf -> t / fromIntegral (fps * tpf)


The midiToEvents function will take a Midi structure (from importFile, 
for example) and convert it to a list of lists of SimpleMsgs. Each outer 
list represents a track in the original Midi. 

> midiToEvents :: Midi -> [[SimpleMsg]]
> midiToEvents m = 
>     let ts = map (simplifyTrack 0) $ map (addTrackTicks 0) (tracks m) 
>     in  distributeTempos $ map (map (applyTD $ timeDiv m)) ts where 
>   simplifyTrack :: Int -> [(Ticks, Message)] -> [SimpleMsg]
>   simplifyTrack icur [] = []
>   simplifyTrack icur ((t,m):ts) = 
>     case m of (NoteOn c p v) -> 
>                   SE (fromIntegral t, p, v, icur, On) : simplifyTrack icur ts
>               (NoteOff c p v) -> 
>                   SE (fromIntegral t, p, v, icur, Off) : simplifyTrack icur ts
>               (ProgramChange c p) -> simplifyTrack (if c==9 then (-1) else p) ts 
>               (TempoChange x) -> T (fromIntegral t, fromIntegral x) : simplifyTrack icur ts
>               _ -> simplifyTrack icur ts 


The first track is the tempo track. It's events need to be distributed
across the other tracks. This function below is called for that purpose
in midiToEvents above.

> distributeTempos :: [[SimpleMsg]] -> [[SimpleMsg]]
> distributeTempos tracks = 
>     if length tracks > 1 then map (sort . (head tracks ++)) (tail tracks)
>     else tracks -- must be a single-track file with embedded tempo changes.


The eventsToMusic function will convert a list of lists of SimpleMsgs 
(output from midiToEvents) to a Music(Pitch,Volume) structure. All 
notes will be connected together using the (:=:) constructor. For 
example, the first line of "Frere Jaque", which would normally be
written as:

c 5 qn :+: d 5 qn :+: e 5 qn :+: c 5 qn

would actually get represented like this when read in from a MIDI:

	(rest 0 :+: c 5 qn) :=:
      (rest qn :+: d 5 qn) :=:
      (rest hn :+: e 5 qn) :=:
      (rest dhn :+: c 5 qn)

This structure is clearly more complicated than it needs to be.
However, identifying melodic lines and phrases inorder to group the
events in a more musically appropriate manor is non-trivial, since
it requires both phrase and voice identification within an instrument 
To see why this is the case, consider a Piano, which may have right 
and lef thand lines that might be best separated by :=: at the 
outermost level. In a MIDI, however, we are likely to get all of the
events for both hands lumped into the same track. 

The parallelized structure is also required for keeping tempo changes
syced between instruments. While MIDI files allow tempo changes to 
occur in the middle of a note, Euterpea's Music values do not.
      
Instruments will be grouped at the outermost level. For example, if 
there are 2 instruments with music values m1 and m2 repsectively, the
structure would be:

    (instrument i1 m1) :=: (instrument i2 m1)
	
Tempo changes are processed within each instrument.

> eventsToMusic :: [[SimpleMsg]] -> Music (Pitch, Volume)
> eventsToMusic tracks = 
>     let tracks' = splitByInstruments tracks -- handle any mid-track program changes
>         is = map toInstr $ map getInstrument $ filter (not.null) tracks' -- instruments
>         tDef = 500000 -- current tempo, 120bpm as microseconds per qn
>     in  chord $ zipWith instrument is $ map (seToMusic tDef) tracks' where
>   
>   toInstr :: Int -> InstrumentName
>   toInstr i = if i<0 then Percussion else toEnum i 
>
>   seToMusic :: Rational -> [SimpleMsg] -> Music (Pitch, Volume)
>   seToMusic tCurr [] = rest 0
>   seToMusic tCurr (e1@(SE(t,p,v,ins,On)):es) = 
>     let piMatch (SE(t1,p1,v1,ins1,e1)) = (p1==p && ins1==ins) && e1==Off
>         piMatch (T(t1,x)) = False
>         is = findIndices piMatch es -- find mactching note-offs
>         SE(t1,p1,v1,ins1, e) = es !! (is !! 0) -- pick the first matching note-off
>         n = (rest t :+: note (t1-t) (pitch p,v)) -- create a Music note
>     in  if v > 0 then -- a zero volume note is silence
>              if length is > 0 then n :=: seToMusic tCurr es -- found an off
>              else seToMusic tCurr ((e1:es)++[correctOff e1 es]) -- missing off case
>         else seToMusic tCurr es
>   seToMusic tCurr (e1@(T (t,newTempo)):es) = 
>     let t2 = getTime $ head es -- find time of next event after tempo change
>         tfact = tCurr / newTempo -- calculate tempo change factor
>         es' = map (changeTime (subtract t)) es -- adjust start times
>         m = rest t :+: tempo tfact (seToMusic newTempo es')  
>     in  if null es then rest 0 else m where
>         changeTime f (SE (t,p,v,i,e)) = SE (f t,p,v,i,e)
>         changeTime f (T (t,x)) = T (f t, x)
>   seToMusic tCurr (_:es) = seToMusic tCurr es -- ignore note-offs (already handled)


Finding the time of an event.

> getTime (SE(t,p,v,i,e)) = t
> getTime (T (t,x)) = t


Finding the instrument associated with a track. Only the first
instrument label to appear is chosen. If a program change happens
mid-track, it will not be counted.

> getInstrument ((SE(t,p,v,i,e)):xs) = i
> getInstrument ((T x) : xs) = getInstrument xs
> getInstrument [] = -1 -- No instrument assigned


The following function ensure that only one instrument appears in 
each list of SimpleMsgs. This is necessary in order to ensure that 
instrument assignments occur at the outermost level of the Music.

> splitByInstruments :: [[SimpleMsg]] -> [[SimpleMsg]] 
> splitByInstruments [] = []
> splitByInstruments (t:ts) = 
>     let i = getInstrument t
>         (t',t'') = splitByI i t
>         ts' = if or $ map isSE t'' then splitByInstruments (t'':ts) 
>               else splitByInstruments ts
>     in  if or $ map isSE t' then t' : ts' else ts'

> isSE :: SimpleMsg -> Bool
> isSE (SE xs) = True
> isSE (T i) = False


The splitByI function partitions a stream to select a specific instrument's events.

> splitByI :: Int -> [SimpleMsg] -> ([SimpleMsg],[SimpleMsg])
> splitByI i0 [] = ([],[])
> splitByI i0 (x:xs) = 
>     let (ts,fs) = splitByI i0 xs
>         f (SE(_,_,_,i1,_)) = i0 == i1
>         f _ = False
>     in  case x of SE x' -> if f x then (x:ts,fs) else (ts,x:fs)
>                   T i -> (x:ts, x:fs) -- add tempos to both streams


This function is an error-handling method for MIDI files which have 
mismatched note on/off events. This seems to be common in output from 
some software. The solution used here is to assume that the note lasts 
until the the time of the last event in the list. 

> correctOff (SE(t,p,v,ins,e)) [] = SE(t,p,v,ins,Off)
> correctOff (SE(t,p,v,ins,e)) es = 
>     let SE(t1,p1,v1,ins1,e1) = last $ filter isSE es
>     in  SE(t1,p,v,ins,Off) 


The fromMidi function wraps the combination of midiToEvents and 
eventsToMusic and performs the final conversion to Music1.

> fromMidi :: Midi -> (Music1, Context (Pitch, [NoteAttribute]), UserPatchMap)
> fromMidi m = 
>     let seList = midiToEvents m
>         iNums = filter (>0) $ map getInstrument seList
>         upm = makeUPM $ map toEnum iNums
>     in  (mMap (\(p,v) -> (p, [Volume v])) $ eventsToMusic seList,
>         defCon, upm)


This function is to correct for the fact that channel 10 is
traditionally reserved for percussion. If there is no percussion,
then channel 10 must remain empty. Channels are indexed from zero 
in this representation, so channel 1 is 0, channel 10 is 9, etc.

> makeUPM :: [InstrumentName] -> UserPatchMap
> makeUPM is = 
>     case findIndex (==Percussion) is of 
>         Nothing -> zip is ([0..8]++[10..]) -- no percussion
>         Just i -> (is !! i, 9) : 
>                   zip (take i is ++ drop (i+1) is) ([0..8]++[10..])

