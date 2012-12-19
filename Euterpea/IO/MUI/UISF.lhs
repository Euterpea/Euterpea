A simple Graphical User Interface with concepts borrowed from Phooey
by Conal Elliot.

> {-# LANGUAGE Arrows, ExistentialQuantification, ScopedTypeVariables, DoRec #-}

> module Euterpea.IO.MUI.UISF where

> import Euterpea.IO.MUI.SOE 
> import Control.Monad (when, unless)
> import qualified Graphics.UI.GLFW as GLFW (sleep, SpecialKey (..))

> import Sound.PortMidi hiding (time)
> import Euterpea.IO.MIDI.MidiIO

> import Control.SF.SF
> import Control.SF.MSF
> import Control.SF.AuxFunctions (toMSF, toRealTimeMSF)
> import Euterpea.IO.MUI.UIMonad

> import Euterpea.IO.Audio.Types (Clock, rate)
> import Control.CCA.ArrowP (ArrowP(..))

> import Prelude hiding (init)
> import Control.Arrow
> import Control.CCA.Types
> import Control.Concurrent.MonadIO
> import Control.DeepSeq


The main UI signal function, built from the UI monad and MSF.

> type UISF = MSF UI


Now that we're using UISF instead of the old UI monad, we should probably get 
rid of the Timer event (of UIMonad's Input data type).  It is only used in 
the following UISF, and with the power of MSF UI, we can get the time directly.

> time :: UISF () Time
> time = MSF f
>  where
>     f _ = do
>       i <- liftIO timeGetTime
>       h i ()
>     h i _ = do
>       t <- liftIO timeGetTime
>       return (t-i, MSF $ h i)


UISF constructors, transformers, and converters
===============================================

These fuctions are various shortcuts for creating UISFs.
The types pretty much say it all for how they work.

> mkUISF :: (a -> (CTX, Focus, Input) -> (Layout, DirtyBit, Focus, Action, [ThreadId], b)) -> UISF a b
> mkUISF f = pipe (\a -> UI $ (\cfa -> return $ f a cfa))

> mkUISF' :: (a -> (CTX, Focus, Input) -> IO (Layout, DirtyBit, Focus, Action, [ThreadId], b)) -> UISF a b
> mkUISF' f = pipe (\a -> UI $ f a)

> expandUISF :: UISF a b -> a -> (CTX, Focus, Input) -> IO (Layout, DirtyBit, Focus, Action, [ThreadId], (b, UISF a b))
> {-# INLINE expandUISF #-}
> expandUISF (MSF f) = unUI . f

> compressUISF :: (a -> (CTX, Focus, Input) -> IO (Layout, DirtyBit, Focus, Action, [ThreadId], (b, UISF a b))) -> UISF a b
> {-# INLINE compressUISF #-}
> compressUISF f = MSF sf
>   where
>     sf a = UI mf
>       where
>         mf cfa = f a cfa

> transformUISF :: (UI (c, UISF b c) -> UI (c, UISF b c)) -> UISF b c -> UISF b c
> transformUISF f (MSF sf) = MSF sf'
>   where
>     sf' a = do
>       (c, nextSF) <- f (sf a)
>       return (c, transformUISF f nextSF)

UISF Lifting
============

The following two functions are for lifting SFs to UISFs.  The first is a 
quick and dirty solution that ignores timing issues.  The second is the 
standard one that appropriately keeps track of simulated time vs real time.  

> toUISF :: SF a b -> UISF a b
> toUISF = toMSF

The clockrate is the simulated rate of the input signal function.
The buffer is the number of time steps the given signal function is allowed 
to get ahead of real time.  The real amount of time that it can get ahead is
the buffer divided by the clockrate seconds.
The output signal function takes and returns values in real time.  The return 
values are the list of bs generated in the given time step, each time stamped.

Note that the returned list may be long if the clockrate is much 
faster than real time and potentially empty if it's slower.
Note also that the caller can check the time stamp on the element 
at the end of the list to see if the inner, "simulated" signal 
function is performing as fast as it should.

> convertToUISF :: forall a b p . (Clock p, NFData b) => Double -> ArrowP SF p a b -> UISF a [(b, Time)]
> convertToUISF buffer (ArrowP sf) = convertToUISF' r buffer sf
>   where r = rate (undefined :: p)

> convertToUISF' :: NFData b => Double -> Double -> SF a b -> UISF a [(b, Time)]
> convertToUISF' clockrate buffer sf = proc a -> do
>   t <- time -< ()
>   toRealTimeMSF clockrate buffer addThreadID sf -< (a, t)


Layout Transformers
===================

Thes functions are UISF transformers that modify the flow in the context.

> topDown, bottomUp, leftRight, rightLeft :: UISF a b -> UISF a b
> topDown   = modifyFlow (\ctx -> ctx {flow = TopDown})
> bottomUp  = modifyFlow (\ctx -> ctx {flow = BottomUp})
> leftRight = modifyFlow (\ctx -> ctx {flow = LeftRight})
> rightLeft = modifyFlow (\ctx -> ctx {flow = RightLeft})
> conjoin   = modifyFlow (\ctx -> ctx {isConjoined = True})
> unconjoin = modifyFlow (\ctx -> ctx {isConjoined = False})

> modifyFlow  :: (CTX -> CTX) -> UISF a b -> UISF a b
> modifyFlow h = transformUISF (modifyFlow' h)

> modifyFlow' :: (CTX -> CTX) -> UI a -> UI a
> modifyFlow' h (UI f) = UI g where g (c,s,i) = f (h c,s,i)


Set a new layout for this widget.

> setLayout  :: Layout -> UISF a b -> UISF a b
> setLayout l = transformUISF (setLayout' l)

> setLayout' :: Layout -> UI a -> UI a
> setLayout' d (UI f) = UI aux
>   where
>     aux inps = do
>       (_, db, foc, a, ts, v) <- f inps
>       return (d, db, foc, a, ts, v)

A convenience function for setLayout, setSize sets the layout to a 
fixed size (in pixels).

> setSize  :: Dimension -> UISF a b -> UISF a b
> setSize (w,h) = setLayout $ makeLayout (Fixed w) (Fixed h)



Add space padding around a widget.

> pad  :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
> pad args = transformUISF (pad' args)

> pad' :: (Int, Int, Int, Int) -> UI a -> UI a
> pad' (w,n,e,s) (UI f) = UI aux
>   where
>     aux (ctx@(CTX i _ m c), foc, inp) = do
>       rec (l, db, foc', a, ts, v) <- f (CTX i ((x + w, y + n),(bw,bh)) m c, foc, inp)
>           let d = l { hFixed = hFixed l + w + e, vFixed = vFixed l + n + s }
>               ((x,y),(bw,bh)) = bounds ctx --computeBBX ctx d
>       return (d, db, foc', a, ts, v)


Execute UI Program
==================

Some default parameters we start with.

> defaultSize :: Dimension
> defaultSize = (300, 300)
> defaultCTX :: Dimension -> (Input -> IO ()) -> CTX
> defaultCTX size inj = CTX TopDown ((0,0), size) inj False
> defaultFocus :: Focus
> defaultFocus = (0, SetFocusTo 0)
> resetFocus (n,SetFocusTo i) = (0, SetFocusTo $ (i+n) `rem` n)
> resetFocus (_,_) = (0,NoFocus)

> runUI   ::              String -> UISF () () -> IO ()
> runUI = runUIEx defaultSize

> runUIEx :: Dimension -> String -> UISF () () -> IO ()
> runUIEx windowSize title sf = runGraphics $ do
>   initialize
>   w <- openWindowEx title (Just (0,0)) (Just windowSize) drawBufferedGraphic
>   (events, addEv) <- makeStream
>   pollEvents <- windowUser w addEv
>   -- poll events before we start to make sure event queue isn't empty
>   pollEvents
>   let uiStream = streamMSF sf (repeat undefined)
>       render drawit' (inp:inps) lastFocus uistream tids = do
>         wSize <- getWindowSize w
>         let ctx = defaultCTX wSize addEv
>         (_, dirty, foc, (graphic, sound), tids', (_, uistream')) <- (unUI $ stream uistream) (ctx, lastFocus, inp)
>         -- always output sound
>         sound
>         -- and delay graphical output when event queue is not empty
>         setGraphic' w graphic
>         let drawit = dirty || drawit'
>             newtids = tids'++tids
>             foc' = resetFocus foc
>         foc' `seq` newtids `seq` case inp of
>           -- Timer only comes in when we are done processing user events
>           Timer _ -> do 
>             -- output graphics 
>             when drawit $ setDirty w
>             quit <- pollEvents
>             if quit then return newtids
>                     else render False inps foc' uistream' newtids
>           _ -> render drawit inps foc' uistream' newtids
>       render _ [] _ _ tids = return tids
>   tids <- render True events defaultFocus uiStream []
>   -- wait a little while before all Midi messages are flushed
>   GLFW.sleep 0.5
>   terminateMidi
>   mapM_ killThread tids
>   closeWindow w

> windowUser w addEv = timeGetTime >>= return . addEvents
>   where
>   addEvents t0 = do 
>     quit <- loop
>     t <- timeGetTime
>     let rt = t - t0
>     addEv (Timer rt)
>     return quit
>   loop = do
>     mev <- maybeGetWindowEvent 0.001 w
>     case mev of
>       Nothing -> return False
>       Just e  -> case e of
> -- There's a bug somewhere with GLFW that makes pressing ESC freeze up 
> -- GHCi, so I've removed this.
> --        SKey GLFW.ESC True -> return True
> --        Key '\00'  True -> return True
>         Closed          -> return True
>         _               -> addEv (UIEvent e) >> loop

> makeStream :: IO ([a], a -> IO ())
> makeStream = do
>   ch <- newChan
>   contents <- getChanContents ch
>   return (contents, writeChan ch)

