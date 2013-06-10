A simple Graphical User Interface with concepts borrowed from Phooey
by Conal Elliot.

> {-# LANGUAGE DoRec #-}

> module Euterpea.IO.MUI.UIMonad where

> import Euterpea.IO.MUI.SOE
> import Control.SF.AuxFunctions (Time)

> import Control.Monad.Fix
> import Control.Concurrent.MonadIO


============================================================
==================== UI Type Definition ====================
============================================================

A UI widget runs under a given context and any focus information from 
earlier widgets and maps input signals to outputs, which consists of 6 parts:
 - its layout,
 - whether the widget needs to be redrawn,
 - any focus information that needs to be conveyed to future widgets
 - the action (to render graphics or/and sounds),
 - any new ThreadIds to keep track of (for proper shutdown when finished),
 - and the parametrized output type.

> newtype UI a = UI 
>   { unUI :: (CTX, Focus, Time, UIEvent) -> 
>             IO (Layout, DirtyBit, Focus, Action, ControlData, a) }


============================================================
======================= Control Data =======================
============================================================

> type ControlData = [ThreadId]
> nullCD :: ControlData
> nullCD = []


> addThreadID :: ThreadId -> UI ()
> addThreadID t = UI (\(_,f,_,_) -> return (nullLayout, False, f, nullAction, [t], ()))

> mergeCD :: ControlData -> ControlData -> ControlData
> mergeCD = (++)


============================================================
===================== Rendering Context ====================
============================================================

A rendering context specifies the following:
 
1. A layout direction to flow widgets. 
 
2. A rectangle bound of current drawing area to render a UI
   component. It specifies the max size of a widget, not the
   actual size. It's up to each individual widget to decide
   where in this bound to put itself.

3. A function that inject inputs into system event queue.
   This is needed to implement some input widgets.

4. A flag to tell whether we are in a conjoined state or not.  
   A conjoined context will duplicate itself for subcomponents 
   rather than splitting.  This can be useful for making compound 
   widgets when one widget takes up space and the other performs 
   some side effect having to do with that space.

> data CTX = CTX 
>   { flow   :: Flow
>   , bounds :: Rect
>   , isConjoined :: Bool
>   }

> data Flow = TopDown | BottomUp | LeftRight | RightLeft deriving (Eq, Show)
> type Dimension = (Int, Int)
> type Rect = (Point, Dimension)


============================================================
========================= UI Layout ========================
============================================================

The layout of a widget provides data to calculate its actual size
in a given context.

> data Layout = Layout
>   { hFill  :: Int
>   , vFill  :: Int
>   , hFixed :: Int
>   , vFixed :: Int
>   , minW   :: Int
>   , minH   :: Int
>   } deriving (Eq, Show)

1. hFill/vFill specify how much stretching space (in units) in
   horizontal/vertical direction should be allocated for this widget.

2. hFixed/vFixed specify how much non-stretching space (width/height in
   pixels) should be allocated for this widget.

3. minW/minH specify minimum values (width/height in pixels) for the widget's 
   dimensions.

Layout calculation makes use of lazy evaluation to do it in one pass.  
Although the UI function maps from Context to Layout, all of the fields of 
Layout must be independent of the Context so that they are avaiable before 
the UI function is even evaluated.

Layouts can end up being quite complicated, but that is usually due to 
layouts being merged (i.e. for multiple widgets used together).  Layouts 
for individual widgets typically come in a few standard flavors, so we 
have the following convenience function for their creation:

----------------
 | makeLayout | 
----------------
This function takes layout information for first the horizontal 
dimension and then the vertical.  A dimension can be either stretchy 
(with a minimum size in pixels) or fixed (measured in pixels).

> data LayoutType = Stretchy { minSize :: Int }
>                 | Fixed { fixedSize :: Int }
>
> makeLayout :: LayoutType -> LayoutType -> Layout
> makeLayout (Fixed h) (Fixed v) = Layout 0 0 h v h v
> makeLayout (Stretchy minW) (Fixed v) = Layout 1 0 0 v minW v
> makeLayout (Fixed h) (Stretchy minH) = Layout 0 1 h 0 h minH
> makeLayout (Stretchy minW) (Stretchy minH) = Layout 1 1 0 0 minW minH

Null layout.

> nullLayout = Layout 0 0 0 0 0 0


============================================================
=============== Context and Layout Functions ===============
============================================================

---------------
 | divideCTX | 
---------------
Divides the CTX according to the ratio of a widget's layout and the 
overall layout of the widget that receives this CTX.  Therefore, the 
first layout argument should basically be a sublayout of the second.

> divideCTX :: CTX -> Layout -> Layout -> (CTX, CTX)
> divideCTX ctx@(CTX a ((x, y), (w, h)) c) 
>           ~(Layout m n u v minw minh) ~(Layout m' n' u' v' minw' minh') =
>   if c then (ctx, ctx) else
>   case a of
>     TopDown   -> (CTX a ((x, y), (w'', h')) c, 
>                   CTX a ((x, y + h'), (w, h - h')) c)
>     BottomUp  -> (CTX a ((x, y + h - h'), (w'', h')) c, 
>                   CTX a ((x, y), (w, h - h')) c)
>     LeftRight -> (CTX a ((x, y), (w', h'')) c, 
>                   CTX a ((x + w', y), (w - w', h)) c)
>     RightLeft -> (CTX a ((x + w - w', y), (w', h'')) c, 
>                   CTX a ((x, y), (w - w', h)) c)
>   where
>     w' = max minw $ (m * div' (w - u') m' + u)
>     h' = max minh $ (n * div' (h - v') n' + v)
>     w'' = max minw $ (if m == 0 then u else w)
>     h'' = max minh $ (if n == 0 then v else h)
>     div' b 0 = 0
>     div' b d = div b d


-----------------
 | mergeLayout | 
-----------------
Merge two layouts into one.

> mergeLayout a (Layout n m u v minw minh) (Layout n' m' u' v' minw' minh') = 
>   case a of
>     TopDown   -> Layout (max' n n') (m + m') (max u u') (v + v') (max minw minw') (minh + minh')
>     BottomUp  -> Layout (max' n n') (m + m') (max u u') (v + v') (max minw minw') (minh + minh')
>     LeftRight -> Layout (n + n') (max' m m') (u + u') (max v v') (minw + minw') (max minh minh')
>     RightLeft -> Layout (n + n') (max' m m') (u + u') (max v v') (minw + minw') (max minh minh')
>   where
>     max' 0 0 = 0
>     max' _ _ = 1


============================================================
================= Action and System State ==================
============================================================

Actions include both Graphics and Sound output. Even though both
are indeed just IO monads, we separate them because Sound output
must be immediately delivered, while graphics can wait until
next screen refresh.

> type Action = (Graphic, Sound)
> type Sound = IO ()
> nullSound = return () :: Sound
> nullAction = (nullGraphic, nullSound) :: Action
> justSoundAction :: Sound -> Action
> justSoundAction s = (nullGraphic, s)
> justGraphicAction :: Graphic -> Action
> justGraphicAction g = (g, nullSound)

> mergeAction (g, s) (g', s') = (overGraphic g' g, s >> s')

> scissorAction :: CTX -> Action -> Action
> scissorAction ctx (g, s) = (scissorGraphic (bounds ctx) g, s)


The Focus and DirtyBit types are for system state.

The Focus type helps focusable widgets communicate with each 
other about which widget is in focus.  It consists of a WidgetID 
and a FocusInfo.  The WidgetID for any given widget is dynamic based 
on how many focusable widgets are active at the moment.  It is designed 
basically as a counter that focusable widgets will automaticall (via the 
focusable function) increment.  The FocusInfo means one of the following.
- A signal of HasFocus indicates that this widget is a subwidget of 
  a widget that is in focus.  Thus, this widget too is in focus, and 
  this widget should pass HasFocus forward.
- A signal of NoFocus indicates that there is no focus information to 
  communicate between widgets.
- A signal of (SetFocusTo n) indicates that the widget whose id equals 
  n should take focus.  That widget should then pass NoFocus onward.

> type Focus = (WidgetID, FocusInfo)
> type WidgetID = Int

> data FocusInfo = HasFocus | NoFocus | SetFocusTo WidgetID
>   deriving (Show, Eq)

The dirty bit is a bit to indicate if the widget needs to be redrawn.

> type DirtyBit = Bool

============================================================
===================== Monadic Instances ====================
============================================================

> instance Monad UI where
>   return i = UI (\(_,foc,_,_) -> return (nullLayout, False, foc, nullAction, nullCD, i))

>   (UI m) >>= f = UI (\(ctx, foc, t, inp) -> do 
>     rec let (ctx1, ctx2)      = divideCTX ctx l1 layout
> --            (ctx2, _)         = divideCTX ctx' l2 l2
>         (l1, db1, foc1, a1, cd1, v1) <- m (ctx1, foc, t, inp)
>         (l2, db2, foc2, a2, cd2, v2) <- unUI (f v1) (ctx2, foc1, t, inp)
>         let action            = (if l1 == nullLayout || l2 == nullLayout then id 
>                                  else scissorAction ctx) $ mergeAction a1 a2
>             layout            = mergeLayout (flow ctx) l1 l2 
>             cd                = mergeCD cd1 cd2
>             dirtybit          = ((||) $! db1) $! db2
>     return (layout, dirtybit, foc2, action, cd, v2))

UIs are also instances of MonadFix so that we can define value
level recursion.

> instance MonadFix UI where
>   mfix f = UI aux
>     where
>       aux (ctx, foc, t, inp) = u
>         where u = do rec (l, db, foc', a, cd, r) <- unUI (f r) (ctx, foc, t, inp)
>                      return (l, db, foc', a, cd, r)

> instance MonadIO UI where
>   liftIO a = UI (\(_,foc,_,_) -> a >>= (\v -> return (nullLayout, False, foc, nullAction, nullCD, v)))

