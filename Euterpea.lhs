> {-# OPTIONS -XFlexibleInstances #-}
> {-# OPTIONS -XTypeSynonymInstances #-}

> module Euterpea (
>   module Euterpea.Music.Note.Music,
>   module Euterpea.Music.Note.MoreMusic,
>   module Euterpea.Music.Note.Performance,
>   module Euterpea.IO.MIDI.GeneralMidi,
>   module Euterpea.IO.MIDI.MidiIO,
>   module Euterpea.IO.MIDI.ToMidi,
>   module Euterpea.IO.MIDI.FromMidi,
>   module Euterpea.IO.Audio
>   -- fromMidi, toMidi,
>   -- mToMF, defUpm, defPMap, defCon, 
>   -- testMidi, test, testA, play, playM, playA,
>   -- makeMidi, gmUpm, gmTest, Performable, perfDur
>   ) where
>
> import Euterpea.Music.Note.Music hiding (t251)
> import Euterpea.Music.Note.MoreMusic
> import Euterpea.Music.Note.Performance
> import Euterpea.IO.MIDI.GeneralMidi
> import Euterpea.IO.MIDI.MidiIO 
> import Euterpea.IO.MIDI.ToMidi
> import Euterpea.IO.MIDI.FromMidi
> import Euterpea.IO.MIDI.FromMidi
> import Euterpea.IO.Audio

> import Codec.Midi(exportFile, importFile, Midi)
> import Sound.PortMidi

To make things easier, we'll declare both Music and Primitive as members of
Functor class. Eventually this will be moved into a separate module.


