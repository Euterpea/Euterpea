

The experimental module for Euterpea includes several features that we believe 
are not yet mature enough for prime time in Euterpea but that we would like 
to include in the project as a whole.  One should not rely on the features 
found here as they may be removed or changed without thought to backwards 
compatability.

> module Euterpea.Experimental (
>     module Euterpea.IO.MUI.InstrumentWidgets
>   -- The InstrumentWidgets module provides support for the piano and guitar
>   -- MUI widgets.
>   , toRealTimeArrow, async
>   -- These conversion functions are for lifting SFs into effectful SFs (i.e. UISFs).
>   , AsyncInput(..), AsyncOutput(..)
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
> ) where

> import Euterpea.IO.MUI.InstrumentWidgets
> import Euterpea.IO.MUI.FFT
> import FRP.UISF.AuxFunctions

> toAutomaton :: SF a b -> Automaton (->) a b
> toAutomaton ~(SF f) = Automaton $ \a -> let (b, sf) = f a in (b, toAutomaton sf)
