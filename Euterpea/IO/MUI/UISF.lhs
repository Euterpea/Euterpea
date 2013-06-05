A simple Graphical User Interface with concepts borrowed from Phooey
by Conal Elliot.

> {-# LANGUAGE Arrows, ExistentialQuantification, ScopedTypeVariables, DoRec, CPP, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}

> module Euterpea.IO.MUI.UISF where

#if __GLASGOW_HASKELL__ >= 610
> import Control.Category
> import Prelude hiding ((.), init, exp)
#else
> import Prelude hiding (init, exp)
#endif
> import Control.Arrow
> import Control.CCA.Types

> import Euterpea.IO.MUI.SOE 
> import Euterpea.IO.MUI.UIMonad

> import Control.SF.SF
> import Control.SF.MSF
> import Control.SF.AuxFunctions (Time, toMSF, toRealTimeMSF, SEvent, ArrowTime (..))
> import Control.CCA.ArrowP (ArrowP(..))
> import Euterpea.IO.Audio.Types (Clock, rate)
> import Euterpea.IO.MIDI.MidiIO (initializeMidi, terminateMidi)

> import Control.Monad (when, unless)
> import Graphics.UI.GLFW (sleep)
> import Control.Concurrent.MonadIO
> import Control.DeepSeq


The main UI signal function, built from the UI monad and MSF.

> type UISF = MSF UI

We probably want this to be a deepseq, but changing the types is a pain.

> instance ArrowInit UISF where
>   init i = MSF (h i) where h i x = seq i $ return (i, MSF (h x))

> instance ArrowTime UISF where
>   time = getTime


============================================================
======================= UISF Getters =======================
============================================================

> getTime      :: UISF () Time
> getTime      = mkUISF (\_ (_,f,t,_) -> (nullLayout, False, f, nullAction, nullCD, t))

> getCTX       :: UISF () CTX
> getCTX       = mkUISF (\_ (c,f,_,_) -> (nullLayout, False, f, nullAction, nullCD, c))

> getEvents    :: UISF () UIEvent
> getEvents    = mkUISF (\_ (_,f,_,e) -> (nullLayout, False, f, nullAction, nullCD, e))

> getFocusData :: UISF () Focus
> getFocusData = mkUISF (\_ (_,f,_,_) -> (nullLayout, False, f, nullAction, nullCD, f))

> getMousePosition :: UISF () Point
> getMousePosition = proc _ -> do
>   e <- getEvents -< ()
>   rec p' <- init (0,0) -< p
>       let p = case e of
>                   MouseMove pt -> pt
>                   _            -> p'
>   returnA -< p


UISF constructors, transformers, and converters
===============================================

These fuctions are various shortcuts for creating UISFs.
The types pretty much say it all for how they work.

> mkUISF :: (a -> (CTX, Focus, Time, UIEvent) -> (Layout, DirtyBit, Focus, Action, ControlData, b)) -> UISF a b
> mkUISF f = pipe (\a -> UI (return . f a))

> mkUISF' :: (a -> (CTX, Focus, Time, UIEvent) -> IO (Layout, DirtyBit, Focus, Action, ControlData, b)) -> UISF a b
> mkUISF' f = pipe (UI . f)

> expandUISF :: UISF a b -> a -> (CTX, Focus, Time, UIEvent) -> IO (Layout, DirtyBit, Focus, Action, ControlData, (b, UISF a b))
> {-# INLINE expandUISF #-}
> expandUISF (MSF f) = unUI . f

> compressUISF :: (a -> (CTX, Focus, Time, UIEvent) -> IO (Layout, DirtyBit, Focus, Action, ControlData, (b, UISF a b))) -> UISF a b
> {-# INLINE compressUISF #-}
> compressUISF f = MSF (UI . f)

> transformUISF :: (UI (c, UISF b c) -> UI (c, UISF b c)) -> UISF b c -> UISF b c
> transformUISF f (MSF sf) = MSF $ \a -> do
>   (c, nextSF) <- f (sf a)
>   return (c, transformUISF f nextSF)

> initialIOAction :: IO x -> (x -> UISF a b) -> UISF a b
> initialIOAction = initialAction . liftIO

source, sink, and pipe functions
DWC Note: I don't feel comfortable with how generic these are.
Also, the continuous ones can't work.

uisfSource :: IO c ->         UISF () c
uisfSink   :: (b -> IO ()) -> UISF b  ()
uisfPipe   :: (b -> IO c) ->  UISF b  c
uisfSource = source . liftIO
uisfSink   = sink . (liftIO .)
uisfPipe   = pipe . (liftIO .)

> uisfSourceE :: IO c ->         UISF (SEvent ()) (SEvent c)
> uisfSinkE   :: (b -> IO ()) -> UISF (SEvent b)  (SEvent ())
> uisfPipeE   :: (b -> IO c) ->  UISF (SEvent b)  (SEvent c)
> uisfSourceE = (init Nothing >>>) . sourceE . liftIO
> uisfSinkE   = (init Nothing >>>) . sinkE . (liftIO .)
> uisfPipeE   = (init Nothing >>>) . pipeE . (liftIO .)



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
>   where modifyFlow' :: (CTX -> CTX) -> UI a -> UI a
>         modifyFlow' h (UI f) = UI g where g (c,s,t,i) = f (h c,s,t,i)


Set a new layout for this widget.

> setLayout  :: Layout -> UISF a b -> UISF a b
> setLayout l = transformUISF (setLayout' l)
>   where setLayout' :: Layout -> UI a -> UI a
>         setLayout' d (UI f) = UI aux
>           where
>             aux inps = do
>               (_, db, foc, a, ts, v) <- f inps
>               return (d, db, foc, a, ts, v)

A convenience function for setLayout, setSize sets the layout to a 
fixed size (in pixels).

> setSize  :: Dimension -> UISF a b -> UISF a b
> setSize (w,h) = setLayout $ makeLayout (Fixed w) (Fixed h)



Add space padding around a widget.

> pad  :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
> pad args = transformUISF (pad' args)
>   where pad' :: (Int, Int, Int, Int) -> UI a -> UI a
>         pad' (w,n,e,s) (UI f) = UI aux
>           where
>             aux (ctx@(CTX i _ c), foc, t, inp) = do
>               rec (l, db, foc', a, ts, v) <- f (CTX i ((x + w, y + n),(bw,bh)) c, foc, t, inp)
>                   let d = l { hFixed = hFixed l + w + e, vFixed = vFixed l + n + s }
>                       ((x,y),(bw,bh)) = bounds ctx
>               return (d, db, foc', a, ts, v)


Execute UI Program
==================

Some default parameters we start with.

> defaultSize :: Dimension
> defaultSize = (300, 300)
> defaultCTX :: Dimension -> CTX
> defaultCTX size = CTX TopDown ((0,0), size) False
> defaultFocus :: Focus
> defaultFocus = (0, SetFocusTo 0)
> resetFocus (n,SetFocusTo i) = (0, SetFocusTo $ (i+n) `rem` n)
> resetFocus (_,_) = (0,NoFocus)

> runUI   ::              String -> UISF () () -> IO ()
> runUI = runUIEx defaultSize

> runUIEx :: Dimension -> String -> UISF () () -> IO ()
> runUIEx windowSize title sf = runGraphics $ do
>   initializeMidi
>   w <- openWindowEx title (Just (0,0)) (Just windowSize) drawBufferedGraphic
>   (events, addEv) <- makeStream
>   let pollEvents = windowUser w addEv
>   -- poll events before we start to make sure event queue isn't empty
>   t0 <- timeGetTime
>   pollEvents
>   let render :: Bool -> [UIEvent] -> Focus -> Stream UI () -> [ThreadId] -> IO [ThreadId]
>       render drawit' (inp:inps) lastFocus uistream tids = do
>         wSize <- getMainWindowSize
>         t <- timeGetTime
>         let rt = t - t0
>         let ctx = defaultCTX wSize
>         (_, dirty, foc, (graphic, sound), tids', (_, uistream')) <- (unUI $ stream uistream) (ctx, lastFocus, rt, inp)
>         -- always output sound
>         sound
>         -- and delay graphical output when event queue is not empty
>         setGraphic' w graphic
>         let drawit = dirty || drawit'
>             newtids = tids'++tids
>             foc' = resetFocus foc
>         foc' `seq` newtids `seq` case inp of
>           -- Timer only comes in when we are done processing user events
>           NoUIEvent -> do 
>             -- output graphics 
>             when drawit $ setDirty w
>             quit <- pollEvents
>             if quit then return newtids
>                     else render False inps foc' uistream' newtids
>           _ -> render drawit inps foc' uistream' newtids
>       render _ [] _ _ tids = return tids
>   tids <- render True events defaultFocus (streamMSF sf (repeat ())) []
>   -- wait a little while before all Midi messages are flushed
>   sleep 0.5
>   terminateMidi
>   mapM_ killThread tids
>   --closeWindow w --unnecessary

> windowUser :: Window -> (UIEvent -> IO ()) -> IO Bool
> windowUser w addEv = do 
>   quit <- loop
>   addEv NoUIEvent
>   return quit
>  where 
>   loop :: IO Bool
>   loop = do
>     mev <- maybeGetWindowEvent 0.001 w
>     case mev of
>       Nothing -> return False
>       Just e  -> case e of
> -- There's a bug somewhere with GLFW that makes pressing ESC freeze up 
> -- GHCi, so I've removed this.
> --        Key (SpecialKey ESC) True _ -> closeWindow w >> return True
>         Closed          -> return True
>         _               -> addEv e >> loop

> makeStream :: IO ([a], a -> IO ())
> makeStream = do
>   ch <- newChan
>   contents <- getChanContents ch
>   return (contents, writeChan ch)

