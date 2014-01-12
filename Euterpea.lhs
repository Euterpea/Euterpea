> {-# OPTIONS -XFlexibleInstances #-}
> {-# OPTIONS -XTypeSynonymInstances #-}

> module Euterpea (
>   module Euterpea.Music.Note.Music,
>   module Euterpea.Music.Note.MoreMusic,
>   module Euterpea.Music.Note.Performance,
>   module Euterpea.IO.Audio,
>   module Euterpea.IO.MIDI,
>   module Euterpea.IO.MUI,
>   module Control.Arrow,
>   -- These 4 lines are from Control.SF.AuxFunctions
>   SEvent, edge, accum, constA, constSF, foldA, foldSF, 
>   unique, hold, now, mergeE,
>   delay, vdelay, fdelay,
>   timer, genEvents, Time, DeltaT,
>   -- This next line is from Codec.Midi
>   exportFile, importFile
>   ) where
>
> import Euterpea.Music.Note.Music hiding (t251)
> import Euterpea.Music.Note.MoreMusic
> import Euterpea.Music.Note.Performance
> import Euterpea.IO.Audio
> import Euterpea.IO.MIDI
> import Euterpea.IO.MUI

> import Control.Arrow
> import Control.SF.AuxFunctions

> import Codec.Midi(exportFile, importFile)
> import Sound.PortMidi



