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

> instance Functor Music where
>   fmap f (Prim x) = Prim (fmap f x)
>   fmap f (x :+: y) = fmap f x :+: fmap f y
>   fmap f (x :=: y) = fmap f x :=: fmap f y
>   -- fmap f (x :=/ y) = fmap f x :=/ fmap f y
>   fmap f (Modify c x) = Modify c (fmap f x)

> instance Functor Primitive where
>   fmap f (Note d x) = Note d (f x)
>   fmap f (Rest d) = Rest d


