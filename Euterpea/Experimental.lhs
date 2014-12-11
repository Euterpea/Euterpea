

The experimental module for Euterpea includes several features that we believe 
are not yet mature enough for prime time in Euterpea but that we would like 
to include in the project as a whole.  One should not rely on the features 
found here as they may be removed or changed without thought to backwards 
compatability.

> {-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

> module Euterpea.Experimental (
>     module Euterpea.IO.MUI.InstrumentWidgets
>   -- The InstrumentWidgets module provides support for the piano and guitar
>   -- MUI widgets.
>   , asyncUISFV, asyncUISFE, clockedSFToUISF, runMIDI
>   -- These conversion functions are for lifting SFs into UISFs.
>   , Automaton(..), toAutomaton
>   -- The async function allows a signal function to run asynchronously.  
>   -- This can be especially useful for a hard computation that needs to be 
>   -- performed sporadically in the MUI.
>   , quantize, presentFFT, fftA
>   -- These functions are used for applying and using the result of a Fast 
>   -- Fourier Transform.
>   , liftAIO       -- :: (b -> IO c) -> a b c
>   , initialAIO    -- :: IO d -> (d -> a b c) -> a b c
>   -- These two functions allow one to lift generic IO actions to a 
>   -- UISF.  They should be used with care.
>   , uisfSource, uisfSink, uisfPipe
>   , uisfSourceE, uisfSinkE, uisfPipeE
> ) where

> import Euterpea.IO.MUI.InstrumentWidgets
> import Euterpea.IO.MUI.FFT
> import FRP.UISF.AuxFunctions
> import FRP.UISF.UISF
> import Control.SF.SF
> import Control.CCA.ArrowP
> import Euterpea.IO.Audio.Types
> import Control.DeepSeq

> import Euterpea.IO.MIDI.MidiIO hiding (Time)
> import Control.Monad (when)
> import Control.Arrow (arr, (>>>), first)
> import Control.Concurrent (killThread)


The below function is useful for making use of asyncUISF*
which both make use of Automatons rather than SFs.
NOTE: Actually, SF and Automaton (->) are the same thing.  Perhaps we should 
      replace our definition of SF with just a type synonym instead.

> toAutomaton :: forall a b . SF a b -> Automaton (->) a b
> toAutomaton ~(SF f) = Automaton $ \a -> let (b, sf) = f a in (b, toAutomaton sf)

The below function is useful for directly asynchronizing AudSFs and CtrSFs in UISF.

> clockedSFToUISF :: forall a b c . (NFData b, Clock c) => Double -> SigFun c a b -> UISF a [(b, Time)]
> clockedSFToUISF buffer ~(ArrowP sf) = let r = rate (undefined :: c) 
>   in asyncUISFV r buffer (toAutomaton sf)

> runMIDI :: (NFData b, NFData c) => (SF (b, SEvent [MidiMessage]) (c, SEvent [MidiMessage])) -> UISF (b, ([DeviceID],[DeviceID])) [c]
> runMIDI sf = asyncC' (addTerminationProc . killThread) (iAction . fst . snd, oAction) sf' where
>   iAction [] = return Nothing
>   iAction (dev:devs) = do
>     valid <- isValidInputDevice dev
>     m <- if valid then pollMidi dev else return Nothing
>     let ret = fmap (\(_t, ms) -> map Std ms) m
>     rst <- iAction devs
>     return $ ret ~++ rst
>   oAction [] = return ()
>   oAction ((dev, ms):rst) = do
>     valid <- isValidOutputDevice dev 
>     when valid $ outputMidi dev >> maybe (return ()) 
>                  (mapM_ $ \m -> deliverMidiEvent dev (0, m)) ms
>     oAction rst
>   sf' = toAutomaton $ arr (\((b,(idevs,odevs)),mms) -> ((b,mms),odevs)) >>> first sf >>>
>           arr (\((c,mms),odevs) -> (c, map (\d -> (d,mms)) odevs))




