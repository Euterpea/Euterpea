
> {-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, CPP #-}

> module Euterpea.IO.MUI.UISFCompat where
> import FRP.UISF.AuxFunctions
> import FRP.UISF.UISF
> import Control.SF.SF
> import Control.Arrow.ArrowP
> import Euterpea.IO.Audio.Types
> import Control.DeepSeq
> import Control.Concurrent (killThread, ThreadId)

#if MIN_VERSION_UISF(0,4,0)
> import FRP.UISF.Asynchrony
> asyncUISFV x = asyncVT x
#endif

The below function is useful for making use of asyncUISF*
which both make use of Automatons rather than SFs.
NOTE: Actually, SF and Automaton (->) are the same thing.  Perhaps we should 
      replace our definition of SF with just a type synonym instead.

> toAutomaton :: forall a b . SF a b -> Automaton (->) a b
> toAutomaton ~(SF f) = Automaton $ \a -> let (b, sf) = f a in (b, toAutomaton sf)

The below function is useful for directly asynchronizing AudSFs and CtrSFs in UISF.

> clockedSFToUISF :: forall a b c . (NFData b, Clock c) => DeltaT -> SigFun c a b -> UISF a [(b, Time)]
> clockedSFToUISF buffer ~(ArrowP sf) = let r = rate (undefined :: c) 
>   in asyncUISFV r buffer (toAutomaton sf)

This function is the standard UISF asynchronous thread handler:

> uisfAsyncThreadHandler :: ThreadId -> UISF a a
> uisfAsyncThreadHandler = addTerminationProc . killThread

