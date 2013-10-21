module Euterpea.IO.MIDI 
  ( 
    fromMidi            -- :: Midi -> (Music1, Context (Pitch, [NoteAttribute]), UserPatchMap)
  , module Euterpea.IO.MIDI.GeneralMidi
--  , fromGM              -- :: Int -> InstrumentName
--  , toGM                -- :: InstrumentName -> Int
  , defaultOutput         -- :: (DeviceID -> a -> IO b) -> a -> IO b
  , defaultInput          -- :: (DeviceID -> a -> IO b) -> a -> IO b
  , playMidi              -- :: DeviceID -> Midi -> IO ()
  , MidiMessage(..)       -- data MidiMessage = ANote { .. } | Std Message
  , Message(..)           -- data Message           (from Codec.Midi)
  , DeviceInfo(..)        -- data DeviceInfo        (from Sound.PortMidi)
  , DeviceID              -- type DeviceID = Int    (from Sound.PortMidi)
  --, exportMidiFile -- :: FilePath -> Midi -> IO ()
  --, writeMidi -- :: (Performable a) => FilePath -> Music a -> IO ()
  --, writeMidiA -- :: (Performable a) => FilePath -> PMap Note1 -> Context Note1 -> Music a -> IO ()
  , module Euterpea.IO.MIDI.ToMidi
  , module Euterpea.IO.MIDI.ExportMidiFile
  ) where

import Euterpea.IO.MIDI.FromMidi
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea.IO.MIDI.ToMidi
import Euterpea.IO.MIDI.ExportMidiFile