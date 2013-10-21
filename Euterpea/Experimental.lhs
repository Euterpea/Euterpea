

The experimental module for Euterpea includes several features that we believe 
are not yet mature enough for prime time in Euterpea but that we would like 
to include in the project as a whole.  One should not rely on the features 
found here as they may be removed or changed without thought to backwards 
compatability.

> module Euterpea.Experimental (
>     module Euterpea.IO.MUI.InstrumentWidgets
>   -- The InstrumentWidgets module provides support for the piano and guitar
>   -- MUI widgets.
>   , toMSF, toRealTimeMSF
>   , toUISF, convertToUISF, convertToUISF'
>   -- These conversion functions are for lifting SFs into MSFs (or UISFs).
>   , async
>   -- The async function allows a signal function to run asynchronously.  
>   -- This can be especially useful for a hard computation that needs to be 
>   -- performed sporadically in the MUI.
>   , quantize, presentFFT, fftA
>   -- These functions are used for applying and using the result of a Fast 
>   -- Fourier Transform.
>   , uisfSourceE         -- :: IO c ->         UISF (SEvent ()) (SEvent c)
>   , uisfSinkE           -- :: (b -> IO ()) -> UISF (SEvent b)  (SEvent ())
>   , uisfPipeE           -- :: (b -> IO c) ->  UISF (SEvent b)  (SEvent c)
>   -- These three functions allow one to lift a generic IO function to a 
>   -- UISF.  They should be used with care.
> ) where

> import Euterpea.IO.MUI.InstrumentWidgets
> import Euterpea.IO.MUI.UISF (toUISF, convertToUISF, convertToUISF',
>                              uisfSourceE, uisfSinkE, uisfPipeE)
> import Control.SF.AuxFunctions (toMSF, toRealTimeMSF, async, quantize, presentFFT, fftA)
