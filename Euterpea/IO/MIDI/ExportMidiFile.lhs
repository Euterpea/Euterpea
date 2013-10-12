MIDI File-writing module for use with Euterpea
Donya Quick
Last modified: 19-June-2013

This file fixes some file-writing bugs in Codec.Midi that 
prevent some multi-instrument output from showing up correctly. 
It defines the function exportMidiFile, which can be used like
Codec.Midi's exportFile function. Additionally, it defines two
functions for writing MIDI files, writeMidi and writeMidiA that
are like test and testA respectively but with an additional file
path argument.

NOTE #1: some of the binary handling should be redone at some 
point. Currently, parts of it are using conversion to a String 
type, and although it works, it should not be necessary (or at 
least a cleaner way should be found).

NOTE #2: many MIDI messages are currently unsupported. The set 
of supported messages is limited to those that can be produced by 
Euterpea.

> module Euterpea.IO.MIDI.ExportMidiFile
>     (exportMidiFile)  where
> import Codec.Midi
> import Numeric
> import Data.Char
> import qualified Data.ByteString as Byte 

A standard MIDI file has two main sections: a header and a 
series of track chunks. Track chunks each have a track header
section and end with an end-of-track marker. Detailed infomation
on the file format can be found here:

http://faydoc.tripod.com/formats/mid.htm


> makeFile :: Midi -> Byte.ByteString
> makeFile (Midi ft td trs) = 
>     let ticksPerQn = 
>             case td of TicksPerBeat x -> x
>                        TicksPerSecond x y -> 
>                            error ("(makeFile) Don't know how "++
>                            "to handle TicksPerSecond yet.")
>         header = makeHeader ft (length trs) ticksPerQn
>         body = map makeTrack trs
>     in  Byte.concat (header:body)

============

BUILD FILE HEADER

The standard MIDI file header starts with the following value:
4D 54 68 00 00 00 06 ff ff nn nn dd dd

ff ff is the format of the file: single-track, multi-track, or 
multi-track/multi-pattern. Only the first two cases are addressed 
here.

nn nn is the number of tracks in the file.

dd dd is the delta-time in ticks for a quarternote or beat.

> midiHeaderConst :: Byte.ByteString
> midiHeaderConst = 
>     Byte.pack [0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06] 

> type TrackCount = Int
> type TicksPerQN = Int


The MIDI file header is built as described above. 

> makeHeader :: FileType -> TrackCount -> TicksPerQN -> Byte.ByteString
> makeHeader ft numTracks ticksPerQn = 
>     let 
>         ft' = case ft of SingleTrack -> [0x00, 0x00]
>                          MultiTrack -> [0x00, 0x01]
>                          MultiPattern -> error ("(makeHeader) Don't know "++
>                                          "how to handle multi-pattern yet.")
>         numTracks' = padByte 2 numTracks
>         ticksPerQn' = padByte 2 ticksPerQn
>     in  if numTracks > 16 then error ("(makeHeader) Don't know how to "++
>                                "handle >16 tracks!")
>         else Byte.concat [midiHeaderConst, Byte.pack ft', numTracks', ticksPerQn']

> padByte :: Integral a => Int -> a -> Byte.ByteString
> padByte byteCount i = 
>   let b = Byte.pack [fromIntegral i] 
>       n = Byte.length b
>       padding = Byte.pack $ take (byteCount - n) $ repeat 0x00
>   in  if n < byteCount then Byte.concat [padding, b] else b

================

BUILDING TRACKS

A track consists of a track header, event information, and an 
end-of-track marker. The track header has the format:

4D 54 72 6B xx xx xx xx

xx xx xx xx is the total number of BYTES in the track that 
follows the header. This includes the end marker! This value
is obtained by generating the track first and then generating
its header.

> makeTrack :: Track Ticks -> Byte.ByteString
> makeTrack t = 
>     let body = makeTrackBody t
>         header = makeTrackHeader body
>     in  Byte.concat [header, body]

> trackHeaderConst :: Byte.ByteString
> trackHeaderConst = Byte.pack [0x4D, 0x54, 0x72, 0x6B] 

> makeTrackHeader :: Byte.ByteString -> Byte.ByteString
> makeTrackHeader tbody = 
>     let len = Byte.length tbody
>         f = Byte.pack . map (fromIntegral . binStrToNum . reverse) . 
>             breakBinStrs 8 . pad (8*4) '0' . numToBinStr
>     in  Byte.concat [trackHeaderConst, f len]

Track events have two components: a variable-length delta-time and
a message. The delta-time is the number of ticks between the last 
message and the next one. The format will be: time message time message ...

However, delta-times are tricky things. The fact that they can be 
any length requires that they be encoded in a special way. The binary
value of the number is split into 7-bit sections. This splitting 
goes from RIGHT TO LEFT (this is not in any documentation I have read,
but was the only way that worked). For n sections, the first start 
with a 1 and the last starts with a 0 - thereby indicating the last 
byte of the number. The following is an example of the conversion:

192 track ticks = C0 (hex) = 1100 0000 (bin) 
==> converts to 8140 (hex)

Split into 7-bit groups:        [1]  [100 0000]
Apply padding:           [000 0001]  [100 0000]
Add flags:              [1000 0001] [0100 0000]
Result as hex               8    1      4    0

> makeTrackBody :: Track Ticks -> Byte.ByteString 
> makeTrackBody [] = endOfTrack -- end marker, very important!
> makeTrackBody ((ticks, msg):rest) = 
>     let b = msgToBytes msg
>         b' = [to7Bits ticks, msgToBytes msg, makeTrackBody rest]
>     in  if Byte.length b > 0 then Byte.concat b'             
>         else makeTrackBody rest

The end of track marker is set 96 ticks after the last event in the 
track. This offset is arbitrary, but it helps avoid clipping the notes
at the end of a file during playback in a program like Winamp or
Quicktime.

> endOfTrack = Byte.concat [to7Bits 96, Byte.pack [0xFF, 0x2F, 0x00]]

Splitting numbers into 7-bit sections and applying flags is done
by the following process:
- convert to a binary string representation
- pad the number to be full bytes
- split from right to left into groups of 7 and apply flags
- convert each 8-bit chunk back to a byte representation

> to7Bits :: (Integral a, Show a) => a -> Byte.ByteString
> to7Bits =  Byte.pack . map (fromIntegral . binStrToNum . reverse) .
>            fixBinStrs . map (padTo 7 . reverse). reverse . 
>            breakBinStrs 7 . reverse . padTo 8 . numToBinStr

Pad a binary string to be a multiple of 8 bits:

> padTo :: Int -> String -> String
> padTo i xs = if length xs `mod` i == 0 then xs else padTo i ('0':xs)

Break a string into chunks of length i:

> breakBinStrs :: Int -> String -> [String]
> breakBinStrs i s = 
>     if length s <= i then [s] else take i s : breakBinStrs i (drop i s)

Convert a number to a binary string:

> numToBinStr :: (Integral a, Show a) => a -> String
> numToBinStr i = showIntAtBase 2 intToDigit i ""

Convert a binary string to an integer:

> binStrToNum :: String -> Int
> binStrToNum [] = 0
> binStrToNum ('0':xs) = 2* binStrToNum xs
> binStrToNum ('1':xs) = 1 + 2*binStrToNum xs
> binStrToNum _ = error "bad data."

Append flags to a string (note, the string must be BACKWARDS):

> fixBinStrs :: [String] -> [String]
> fixBinStrs xs = 
>     let n = length xs
>         bits = take (n-1) (repeat '1') ++ "0"
>     in  Prelude.zipWith (:) bits xs

Pad a list from the left until it is a fixed length:

> pad :: Int -> a -> [a] -> [a]
> pad b x xs = if length xs >= b then xs else pad b x (x:xs)

Messages have the following encodings:

8x nn vv	Note Off for pitch nn at velocity vv, channel x
9x nn vv	Note On for pitch nn at velocity vv, channel x
Ax nn vv	Key aftertouch for pitch nn at velocity vv, channel x
Bx cc vv	Control Change for controller cc with value vv, channel x
Cx pp		Program Change to patch pp for channel x
Dx cc 		Channel after-touch to cc on channel x
Ex bb tt 	Pitch wheel to value ttbb, channel x (2000 hex is "normal") 
            (note: bb are least significant bits, tt are most significant)

Currently, only note on/off, control change, and program change are supported.

There are also META -EVENTS. This are events that have no channel number.
All meta-events have the format

FF xx nn nn dd dd ...

where xx is the command code, and nnnn is the number of bytes in the data (dd).

FF 00 nn ssss		Set track sequence number
FF 01 nn tt...		Text event
FF 02 nn tt...		Copyright info
FF 03 nn tt...		Track name
FF 04 nn tt...		Track instrument name
FF 05 nn tt...		Lyric
FF 06 nn tt...		Marker
FF 07 nn tt...		Cue point
FF 2F 00			END OF TRACK MARKER
FF 51 03 tttttt		Tempo change marker, where tttttt is the microseconds per qn
FF 48 04 nnddccbb	Time signature nn/dd with cc ticks per beat and bb 32nds/qn
FF 59 02 sfmi		Key signature with sf sharps/flats and mi mode in {0,1}

Of these, only the end of track and tempo marker are implemented.

> msgToBytes :: Message -> Byte.ByteString
> msgToBytes (NoteOn c k v) = 
>     Byte.concat [Byte.pack [0x90 + fromIntegral c], padByte 1 k, padByte 1 v]
> msgToBytes (NoteOff c k v) = 
>     Byte.concat [Byte.pack [0x80 + fromIntegral c], padByte 1 k, padByte 1 v]
> msgToBytes (ProgramChange c p) =  
>     Byte.concat [Byte.pack [0xC0 + fromIntegral c], padByte 1 p]
> msgToBytes (ControlChange c n v) =  
>     Byte.concat [Byte.pack [0xB0 + fromIntegral c], padByte 1 n, padByte 1 v]
> msgToBytes (TempoChange t) = -- META EVENT, HAS NO CHANNEL NUMBER
>     Byte.concat [Byte.pack [0xFF, 0x51, 0x03], fixTempo t]
> msgToBytes x = error ("(msgToBytes) Message type not currently "++ 
>                "supported: "++show x)

Fix a tempo value to be exactly 3 bytes:

> fixTempo = Byte.pack . map (fromIntegral . binStrToNum . reverse) . 
>            breakBinStrs 8 . pad (4*6) '0' . numToBinStr

> exportMidiFile :: FilePath -> Midi -> IO ()
> exportMidiFile fn = Byte.writeFile fn . makeFile


=================

USAGE

The exportMidiFile can now be used as follows in place of Codec.Midi's exportFile:

 writeMidi :: (Performable a) => FilePath -> Music a -> IO ()
 writeMidi fn = exportMidiFile fn . testMidi

 writeMidiA :: (Performable a) => FilePath -> PMap Note1 -> Context Note1 -> Music a -> IO ()
 writeMidiA fn pm con m = exportMidiFile fn $ testMidiA pm con m

 test :: (Performable a) => Music a -> IO ()
 test = exportMidiFile "test.mid" . testMidi
 
 testA :: Performable a => PMap Note1 -> Context Note1 -> Music a -> IO ()
 testA pm con m = exportMidiFile "test.mid" (testMidiA pm con m)
 