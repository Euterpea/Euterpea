A simple Graphical User Interface based on FRP. It uses the SOE
graphics library, and draws custom widgets on the screen.

SOE graphics uses OpenGL as the primitive drawing routine, and
GLFW library to provide window and input support.

The monadic UI concept is borrowed from Phooey by Conal Elliott.

> {-# LANGUAGE DoRec, Arrows #-}

> module Euterpea.IO.MUI.Widget where

> import Euterpea.IO.MUI.UIMonad
> import Euterpea.IO.MUI.UISF
> import Euterpea.IO.MUI.SOE hiding (Event)
> import System.IO.Unsafe (unsafePerformIO)
> import Control.Monad (when)
> import Euterpea.IO.MIDI.MidiIO
> import Sound.PortMidi hiding (time)

> import Control.SF.AuxFunctions

> import Prelude hiding (init)
> import Control.Arrow
> import Control.CCA.Types

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Char (isPrint, ord)

============================================================
============== Shorthand and Helper Functions ==============
============================================================

Default padding between border and content

> padding = 3 

Introduce a shorthand for overGraphic

> (//) = overGraphic

A helper function to make stateful Widgets easier to write.

> mkWidget :: s ->                                  -- initial state
>         Layout ->                                 -- layout
>         (Rect -> s -> Graphic) ->                 -- drawing routine
>         (s -> Sound) ->                           -- sound routine
>         (a -> s -> s1) ->                         -- input injection
>         ((s1, (CTX, Sys, Input)) -> (s2, Sys)) -> -- computation
>         (s2 -> (b, s)) ->                         -- output projection
>         UISF a b
> mkWidget i layout draw buzz inj comp prj = proc a -> do
>   rec s  <- init i -< s'
>       (y, s') <- mkUISF aux -< inj a s
>   returnA -< y
>   --loop $ second (init i) >>> arr (uncurry inj) >>> mkUISF aux
>     where
>       aux s1 (ctx,sys,inp) = (layout, sys', action, [], (y, s'))
>         where
>           (s2, sys') = comp (s1, (ctx, sys, inp))
>           (y, s') = prj s2
>           action = (draw bbx `cross` buzz) s'
>           bbx = computeBBX ctx layout

A few useful shorthands for creating widgets with mkWidget

> dup x = (x, x)
> pair = (,)
> markDirty sys d = sys { dirty = dirty sys || d }



============================================================
========================= Widgets ==========================
============================================================

----------------
 | Text Label | 
----------------
Labels are always left aligned and vertically centered.

> label :: String -> UISF a a
> label s = mkUISF aux
>   where
>     (minw, minh) = (length s * 8 + padding * 2, 16 + padding * 2)
>     d = makeLayout (Fixed minw) (Fixed minh)
>     drawit ((x, y), (w, h)) = withColor Black $ 
>       text (x + padding, y + padding) s
>     aux a (ctx,sys,inp) = (d, sys, action, [], a)
>       where 
>         action = justGraphicAction $ drawit bbx
>         bbx = computeBBX ctx d

-----------------
 | Display Box | 
-----------------
Display is an output widget showing the instantaneous value of
a signal of strings.

> display :: UISF String ()
> display = mkWidget "" d draw (const nullSound) pair 
>   (\((v, v'), (_, sys, _)) -> (v, markDirty sys (v /= v')))
>   (\s -> ((), s))
>   where
>     minh = 16 + padding * 2
>     d = makeLayout (Stretchy 8) (Fixed minh)
>     draw b@(p@(x,y), (w, h)) s = 
>       let n = (w - padding * 2) `div` 8
>       in withColor Black (text (x + padding, y + padding) (take n s)) // 
>          (box pushed b) // (withColor White $ block b) 

display' is a widget that takes any show-able value and displays it.

> display' :: Show a => UISF a ()
> display' = arr show >>> display

withDisplay is a widget modifier that modifies the given widget 
so that it also displays its output value.

> withDisplay :: Show b => UISF a b -> UISF a b
> withDisplay sf = proc a -> do
>   b <- sf -< a
>   _ <- display -< show b
>   returnA -< b


--------------
 | Text Box | 
--------------
Textbox is a widget showing the instantaneous value of a signal of 
strings.  It takes two static arguments:
startingFocus - A boolean representing whether this 
    textbox starts with focus
startingVal - The initial value in the textbox

The textbox widget will often be used with ArrowLoop (the rec keyword).  
However, it uses init internally, so there should be no fear of a blackhole.

> textbox :: Bool -> String -> UISF String String
> textbox startingFocus startingVal = withFocusStatus startingFocus $ proc (inFocus, s) -> do
>   k <- getKeyStrokes -< ()
>   s' <- init startingVal -< if inFocus then amend s k else s
>   _ <- display  -< s'
>   returnA -< s'
>  where
>   amend s (Just (c, True)) | ord c == 8 = take (length s - 1) s
>                            | otherwise  = s ++ [c]
>   amend s _ = s


-----------
 | Title | 
-----------
Title frames a UI by borders, and displays a static title text.

> title :: String -> UISF a b -> UISF a b
> title label sf = compressUISF (modsf sf)
>   where
>     (tw, th) = (length label * 8, 16)
>     drawit ((x, y), (w, h)) g = 
>       withColor Black (text (x + 10, y) label) //
>       (withColor' bg $ block ((x + 8, y), (tw + 4, th))) //
>       box marked ((x, y + 8), (w, h - 16)) // g
>     modsf sf a (ctx@(CTX _ bbx@((x,y), (w,h)) myid m _),sys,inp) = do
>       (l,sys',action,ts,(v,nextSF)) <- expandUISF sf a (CTX TopDown ((x + 4, y + 20), (w - 8, h - 32))
>                                   (pushWidgetID myid) m False, sys, inp)
>       let d = l { hFixed = hFixed l + 8, vFixed = vFixed l + 36, 
>                   minW = max (tw + 20) (minW l), minH = max 36 (minH l) }
>       return (d, sys', (\(g, s) -> (drawit bbx g, s)) action, ts, (v,compressUISF (modsf nextSF)))


------------
 | Button | 
------------
Button is an input widget with a state of being on or off,
it also shows a static text label.

> button :: String -> UISF () Bool
> button label = 
>   mkWidget False d draw (const nullSound) (const id)
>        process dup
>   where
>     (tw, th) = (8 * length label, 16) 
>     (minw, minh) = (tw + padding * 2, th + padding * 2)
>     d = makeLayout (Stretchy minw) (Fixed minh)
>     draw b@((x,y), (w,h)) down = 
>       let x' = x + (w - tw) `div` 2 + if down then 0 else -1
>           y' = y + (h - th) `div` 2 + if down then 0 else -1
>       in (withColor Black $ text (x', y') label) // 
>          (box (if down then pushed else popped) b)
>     process (s, (ctx, sys, evt)) = (s', markDirty sys' (s /= s'))
>       where 
>         (s', sys') = case evt of
>           UIEvent (Button pt True down) -> case (focused, s, down) of
>             (True, False, True) -> (True, sys)
>             (True, True, False) -> (False, sys)
>             _ -> (s, sys)
>           UIEvent (MouseMove pt) -> if pt `inside` bbx 
>             then (s, if focused then sys else sys { nextFocus = Just myid })
>             else (False, if focused then sys { focus = Nothing } else sys)
>           _ -> (s, sys) 
>           where
>             bbx = bounds ctx --computeBBX ctx d
>             myid = uid ctx 
>             focused = focus sys == Just myid


---------------
 | Check Box | 
---------------
Checkbox allows selection or deselection of an item.
It has a static label as well as an initial state.

> checkbox :: String -> Bool -> UISF () Bool
> checkbox label state = proc _ -> do
>   rec s  <- init state -< s'
>       e  <- edge <<< toggle state d draw -< s
>       let s' = if e then not s else s --maybe s (\_ -> not s) e
>   returnA -< s'
>   where
>     (tw, th) = (8 * length label, 16) 
>     (minw, minh) = (tw + padding * 2, th + padding * 2)
>     d = makeLayout (Stretchy minw) (Fixed minh)
>     draw ((x,y), (w,h)) down = 
>       let x' = x + padding + 16 
>           y' = y + (h - th) `div` 2
>           b = ((x + padding + 2, y + h `div` 2 - 6), (12, 12))
>       in (withColor Black $ text (x', y') label) // 
>          (if down 
>             then withColor' gray3 (polyline 
>               [(x + padding + 5, y + h `div` 2),
>                (x + padding + 7, y + h `div` 2 + 3),
>                (x + padding + 11, y + h `div` 2 - 2)])
>             else nullGraphic) //
>       box pushed b // (withColor White $ block b)

-------------------
 | Radio Buttons | 
-------------------
Radio button presents a list of choices and only one of them can be 
selected at a time.  It takes a static list of choices (as Strings) 
and the index of the initially selected one, and the widget itself 
returns the continuous stream representing the index of the selected 
choice.

> radio :: [String] -> Int -> UISF () Int
> radio labels i = proc _ -> do
>   rec s   <- init i -< s''
>       s'  <- aux 0 labels -< s
>       let s'' = maybe s id s'
>   returnA -< s''
>   where
>     aux :: Int -> [String] -> UISF Int (Event Int)
>     aux j [] = arr (const Nothing)
>     aux j (l:labels) = proc n -> do
>       u <- edge <<< toggle (i == j) d draw -< n == j
>       v <- aux (j + 1) labels -< n
>       returnA -< if u then Just j else v --maybe v (const $ Just j) u
>       where
>         (tw, th) = (8 * length l, 16) 
>         (minw, minh) = (tw + padding * 2, th + padding * 2)
>         d = makeLayout (Stretchy minw) (Fixed minh)
>         draw ((x,y), (w,h)) down = 
>           let x' = x + padding + 16 
>               y' = y + (h - th) `div` 2
>           in (withColor Black $ text (x', y') l) // 
>              (if down then withColor' gray3 $ arc (x + padding + 5, y + padding + 6) 
>                                            (x + padding + 9, y + padding + 10) 0 360
>                       else nullGraphic) //
>              (withColor' gray3 $ arc (x + padding + 2, y + padding + 3) 
>                                      (x + padding + 12, y + padding + 13) 0 360) //
>              (withColor' gray0 $ arc (x + padding + 2, y + padding + 3) 
>                                      (x + padding + 13, y + padding + 14) 0 360)


-------------
 | Sliders | 
-------------

Sliders are input widgets that allow the user to choose a value within 
a given range.  They come in both continous and discrete flavors as well 
as in both vertical and horizontal layouts.

Sliders take a boundary argument giving the minimum and maximum possible 
values for the output as well as an initial value.  In addition, discrete 
(or integral) sliders take a step size as their first argument.

> hSlider, vSlider :: RealFrac a => (a, a) -> a -> UISF () a
> hSlider = slider True     -- Horizontal Continuous Slider
> vSlider = slider False    -- Vertical Continuous Slider
> hiSlider, viSlider :: Integral a => a -> (a, a) -> a -> UISF () a
> hiSlider = iSlider True   -- Horizontal Discrete Slider
> viSlider = iSlider False  -- Vertical Discrete Slider

> slider :: RealFrac a => Bool -> (a, a) -> a -> UISF () a
> slider hori (min, max) = mkSlider hori v2p p2v jump
>   where
>     v2p v w = truncate ((v - min) / (max - min) * fromIntegral w)
>     p2v p w =  
>       let v = min + (fromIntegral (p - padding) / fromIntegral w * (max - min))
>       in if v < min then min else if v > max then max else v
>     jump d w v = 
>       let v' = v + fromIntegral d * (max - min) * 16 / fromIntegral w
>       in if v' < min then min else if v' > max then max else v'

> iSlider hori step (min, max) = mkSlider hori v2p p2v jump
>   where
>     v2p v w = w * fromIntegral (v - min) `div` fromIntegral (max - min)
>     p2v p w =  
>       let v = min + fromIntegral (round (fromIntegral (max - min) * 
>               fromIntegral (p - padding) / fromIntegral w))
>       in if v < min then min else if v > max then max else v
>     jump d w v = 
>       let v' = v + step * fromIntegral d 
>       in if v' < min then min else if v' > max then max else v'


---------------------
 | Real Time Graph | 
---------------------
The realtimeGraph widget creates a graph of the data with trailing values.  
It takes a dimension parameter, the length of the history of the graph 
measured in time, and a color for the graphed line.
The signal function then takes an input stream of time as well as 
(value,time) event pairs, but since there can be zero or more points 
at once, we use [] rather than Event for the type.
The values in the (value,time) event pairs should be between -1 and 1.

The below two implementation of realtimeGraph produce the same output, 
but the first one performs better for some reason.  I'm not sure why ...

> realtimeGraph :: RealFrac a => Layout -> Time -> Color -> UISF (Time, [(a,Time)]) ()
> realtimeGraph layout hist color = 
>   mkWidget ([(0,0)],0) layout draw (const nullSound) (,) process (\s -> ((), s))
>   where draw ((x,y), (w, h)) (lst,t) = translateGraphic (x,y) $ 
>           if null lst then nullGraphic else withColor color $ polyline (map (adjust t) lst)
>           where adjust t (i,t0) = (truncate $ fromIntegral w * (hist + t0 - t) / hist,
>                                    buffer + truncate (fromIntegral (h - 2*buffer) * (1 + i)/2))
>                 buffer = truncate $ fromIntegral h / 10
>         removeOld _ [] = []
>         removeOld t ((i,t0):is) = if t0+hist>=t then (i,t0):is else removeOld t is
>         process (((t,is),(lst,_)), (_, sys, _)) = ((removeOld t (lst ++ is), t), markDirty sys True)

 realtimeGraph :: RealFrac a => Layout -> Time -> Color -> UISF (Time, [(a,Time)]) ()
 realtimeGraph layout hist color = proc (t, is) -> do
   rec lst <- init [(0,0)] -< removeOld t (lst ++ is)
   canvas' layout draw -< if null lst then Nothing else Just (lst, t)
   where removeOld _ [] = []
         removeOld t ((i,t0):is) = if t0+hist>=t then (i,t0):is else removeOld t is
         draw (lst,t) (w,h) = withColor color $ polyline (map (adjust t) lst) where
           adjust t (i,t0) = (truncate $ fromIntegral w * (hist + t0 - t) / hist,
                              buffer + truncate (fromIntegral (h - 2*buffer) * (1 - i)))
           buffer = truncate $ fromIntegral h / 10


---------------
 | Histogram | 
---------------
The histogram widget creates a histogram of the input map.  It assumes 
that the elements are to be displayed linearly and evenly spaced.

Similar to realtimeGraph above, these two histograms are the same, but 
the first one performs better for some reason.

> histogram :: RealFrac a => Layout -> UISF (Event [a]) ()
> histogram layout = 
>   mkWidget Nothing layout draw (const nullSound) inj process (\s -> ((), s))
>   where inj inp prev = maybe prev Just inp
>         process (Nothing, (_, sys, _)) = (Nothing, sys)
>         process (Just a,  (_, sys, _)) = (Just a, markDirty sys True)
>         draw ((x,y), (w, h)) = translateGraphic (x,y) . mymap (polyline . mkPts)
>           where mkPts l  = zip (xs $ length l) (map adjust . normalize . reverse $ l)
>                 xs n     = reverse $ map truncate [0,(fromIntegral w / fromIntegral (n-1))..(fromIntegral w)]
>                 adjust i = buffer + truncate (fromIntegral (h - 2*buffer) * (1 - i))
>                 normalize lst = map (/m) lst where m = maximum lst
>                 buffer = truncate $ fromIntegral h / 10
>                 mymap :: ([a] -> Graphic) -> Event [a] -> Graphic
>                 mymap f (Just lst@(_:_)) = f lst
>                 mymap _ _ = nullGraphic

 histogram :: RealFrac a => Layout -> UISF (Event [a]) ()
 histogram layout = canvas' layout draw where
   draw lst (w,h) = maybeEmptyList nullGraphic (polyline . mkPts) lst where
       mkPts l  = zip (xs $ length l) (map adjust . normalize . reverse $ l)
       xs n     = reverse $ map truncate [0,(fromIntegral w / fromIntegral (n-1))..(fromIntegral w)]
       adjust i = buffer + truncate (fromIntegral (h - 2*buffer) * (1 - i))
       normalize lst = map (/m) lst where m = maximum lst
       maybeEmptyList :: b -> ([a] -> b) -> [a] -> b
       maybeEmptyList _ f lst@(_:_) = f lst
       maybeEmptyList b _ [] = b
       buffer = truncate $ fromIntegral h / 10


-------------------
 | Midi Controls | 
-------------------
midiIn is a widget that accepts a MIDI device ID and returns the event 
stream of MidiMessages that that device is producing.

midiOut is a widget that accepts a MIDI device ID as well as a stream 
of MidiMessages and sends the MidiMessages to the device.

> midiIn :: UISF DeviceID (Event [MidiMessage])
> midiIn = mkUISF aux 
>   where 
>     aux dev (ctx,sys,inp) = (nullLayout, sys, action, [], v)
>       where 
>         v = case inp of
>               MidiEvent d m -> if d == dev
>                                then Just [Std m]
>                                else Nothing
>               _ -> Nothing
>         action = justSoundAction $ do
>           valid <- isValidInputDevice dev
>           when valid $ pollMidi dev (cb dev)
>         cb d (t, m) = inject ctx (MidiEvent d m)
 
> midiOut :: UISF (DeviceID, Event [MidiMessage]) ()
> midiOut = mkUISF aux 
>   where
>     aux (dev, mmsg) (ctx,sys,_) = (nullLayout, sys, action, [], ())
>       where
>         action = justSoundAction $ do
>           valid <- isValidOutputDevice dev 
>           when valid $ case mmsg of
>                 Just msgs -> tryOutputMidi dev >>
>                              mapM_ (\m -> outputMidi dev (0, m)) msgs
>                 Nothing   -> tryOutputMidi dev


----------------------
 | Device Selection | 
----------------------
selectInput and selectOutput are shortcut widgets for producing a set 
of radio buttons corresponding to the available input and output devices 
respectively.  The output is the DeviceID for the chosen device rather 
that just the radio button index as the radio widget would return.

> selectInput, selectOutput :: UISF () DeviceID
> selectInput = selectDev "Input device" input
> selectOutput = selectDev "Output device" output

> selectDev :: String -> (DeviceInfo -> Bool) -> UISF () DeviceID
> selectDev t f = title t $ proc _ -> do
>   r <- radio (map name $ snd $ unzip devs) defaultChoice -< ()
>   let devId = if r == -1 then r else fst (devs !! r)
>   --_ <- display -< show devId
>   returnA -< devId
>       where devs = filter (\(i,d) -> f d && name d /= "Microsoft MIDI Mapper") $ 
>                      zip [0..] $ unsafePerformIO getAllDeviceInfo
>             defaultChoice = if null devs then (-1) else 0



============================================================
===================== Widget Builders ======================
============================================================

----------------------
 | Toggle | 
----------------------
The toggle is useful in the creation of both checkboxes and radio 
buttons.  It displays on/off according to its input, and when the mouse 
is clicked on it, it will output True (otherwise it outputs False).

The UISF returned from a call to toggle accepts the state stream and 
returns whether the toggle is being clicked.

> toggle :: (Eq s) => s             -- Initial state value
>        -> Layout                  -- The layout for the toggle
>        -> (Rect -> s -> Graphic)  -- The drawing routine
>        -> UISF s Bool
> toggle init layout draw = 
>   mkWidget init layout draw (const nullSound) pair process id
>   where
>     process ((s,s'), (ctx, sys, evt)) = ((on,s), markDirty sys' (s /= s'))
>       where 
>         (on, sys') = case evt of
>           UIEvent (Button pt True down) | pt `inside` bbx -> (down, sys)
>           _ -> (False, sys) 
>           where
>             bbx = computeBBX ctx layout

--------------
 | mkSlider | 
--------------
The mkSlider widget builder is useful in the creation of all sliders.

> mkSlider :: Eq a => Bool              -- True for horizontal, False for vertical
>          -> (a -> Int -> Int)         -- A function for converting a value to a position
>          -> (Int -> Int -> a)         -- A function for converting a position to a value
>          -> (Int -> Int -> a -> a)    -- A function for determining how much to jump when 
>                                       -- a click is on the slider but not the target
>          -> a                         -- The initial value for the slider
>          -> UISF () a
> mkSlider hori val2pos pos2val jump val0 = 
>   mkWidget (val0, Nothing) d draw (const nullSound) (const id)
>        process (\s -> (fst s, s))
>   where
>     rotP p@(x,y) ((bx,by),_) = if hori then p else (bx + y - by, by + x - bx)
>     rotR r@(p@(x,y),(w,h)) bbx = if hori then r else (rotP p bbx, (h,w))
>     (minw, minh) = (16 + padding * 2, 16 + padding * 2)
>     (tw, th) = (16, 8)
>     d = if hori then Layout (Stretchy minw) (Fixed minh)
>                 else Layout (Fixed minh) (Stretchy minw)
>     val2pt val ((bx,by), (bw,bh)) = 
>       let p = val2pos val (bw - padding * 2 - tw)
>       in (bx + p + padding, by + 8 - th `div` 2 + padding) 
>     bar ((x,y),(w,h)) = ((x + padding + tw `div` 2, y + 6 + padding), 
>                          (w - tw - padding * 2, 4))
>     draw b@((x,y), (w, h)) (val, _) = 
>       let p@(mx,my) = val2pt val (rotR b b)
>       in box popped (rotR (p, (tw, th)) b)
>          // (withColor' bg $ block $ rotR ((mx + 2, my + 2), (tw - 4, th - 4)) b)
>          // (box pushed $ rotR (bar (rotR b b)) b)
>     process ((val, s), (ctx, sys, evt)) = ((val', s'), markDirty sys' (val /= val'))
>       where
>         ((val', s'), sys') = case evt of
>           UIEvent (Button pt' True down) -> let pt = rotP pt' bbx in
>             case (pt `inside` target, down) of
>               (True, True) -> ((val, Just (ptDiff pt val)), 
>                                if focused then sys else sys { nextFocus = Just myid })
>               (_, False)   -> ((val, Nothing), 
>                                if focused then sys { focus = Nothing } else sys)
>               (False, True) | pt `inside` bar' -> clickonbar pt
>               _ -> ((val, s), sys) 
>           UIEvent (MouseMove pt') -> let pt = rotP pt' bbx in
>             case s of
>               Just pd -> ((pt2val pd pt, Just pd), sys)
>               Nothing -> ((val, s), sys)
>           _ -> ((val, s), sys) 
>         bbx@((bx,by),(bw,bh)) = let b = computeBBX ctx d in rotR b b
>         bar' = let ((x,y),(w,h)) = bar bbx in ((x,y-4),(w,h+8))
>         myid = uid ctx 
>         focused = focus sys == Just myid
>         target = (val2pt val bbx, (tw, th)) 
>         ptDiff (x, y) val = 
>           let (x', y') = val2pt val bbx
>           in (x' + tw `div` 2 - x, y' + th `div` 2 - x)
>         pt2val (dx, dy) (x,y) = pos2val (x + dx - bx - tw `div` 2) (bw - 2 * padding - tw)
>         clickonbar pt@(x',y') = 
>           let (x,y) = val2pt val bbx
>               val' = jump (if x' < x then -1 else 1) bw val
>           in ((val', s), sys)

------------
 | Canvas | 
------------
Canvas displays any graphics. The input is a signal of graphics
event because we only want to redraw the screen when the input
is there.

> canvas :: Dimension -> UISF (Event Graphic) ()
> canvas (w, h) = mkWidget nullGraphic layout draw (const nullSound)
>                 pair process (\s -> ((), s)) 
>   where
>     layout = makeLayout (Fixed w) (Fixed h)
>     draw ((x,y),(w,h)) = translateGraphic (x,y)
>     process ((u, g), (_, sys, _)) = case u of
>       Just g' -> (g', markDirty sys True)
>       Nothing -> (g, sys)

canvas' uses a layout and a graphic generator which allows canvas to be 
used in cases with stretchy layouts.

> canvas' :: Layout -> (a -> Dimension -> Graphic) -> UISF (Event a) ()
> canvas' layout draw = mkWidget Nothing layout drawit (const nullSound)
>                       pair process (\s -> ((), s))
>   where
>     drawit (pt, dim) ea = maybe nullGraphic (\a -> translateGraphic pt $ draw a dim) ea
>     process ((ea, a'), (_, sys, _)) = case ea of
>       Just a  -> (Just a,  markDirty sys True)
>       Nothing -> (a', sys)



============================================================
========================= Utilities ========================
============================================================

-----------------------------
 | Variable Duration Timer | 
-----------------------------
This timer takes the current time as well as the (variable) time between 
events and returns a Bool steam representing an event stream (where an 
event is simply a True output).  When the second argument is non-positive, 
the output will be a steady stream of True.  As long as the clock speed is 
fast enough compared to the timer frequency, this should give accurate and 
predictable output and stay synchronized with any other timer and with 
time itself.


> timer :: ArrowInit a => a (Time, Double) Bool
> timer = proc (now, i) -> do
>   rec last <- init 0 -< t'
>       let ret = now >= last + i
>           t'  = latestEventTime last i now
>   returnA -< ret
>  where
>   latestEventTime last i now | i <= 0 = now
>   latestEventTime last i now = 
>       if now > last + i
>       then latestEventTime (last+i) i now
>       else last


NOTE: The following two functions may be better off with Data.Sequence

-----------------
 | Fixed Delay | 
-----------------
Given a time to delay, this returns a signal function that takes the 
current time and an event stream and delays the event stream by the 
delay amount.

> delay :: ArrowInit a => Double -> a (Time, Event b) (Event b)
> delay d = proc (t, e) -> do
>   rec q <- init [] -< maybe q' (\e' -> q' ++ [(t+d,e')]) e
>       let (ret, q') = case q of
>               [] -> (Nothing, q)
>               (t0,e0):qs -> if t >= t0 then (Just e0, qs) else (Nothing, q)
>   returnA -< ret


--------------------
 | Variable Delay | 
--------------------
This is a signal function that takes the current time, an amount of time 
to delay, and an event stream and delays the event stream by the 
delay amount.

> delayt :: ArrowInit a => a (Time, Double, Event b) (Event b)
> delayt = proc (t, d, e) -> do
>   rec q <- init [] -< maybe q' (\e' -> q' ++ [(t,e')]) e
>       let (ret, q') = case q of
>               [] -> (Nothing, q)
>               (t0,e0):qs -> if t-t0 >= d then (Just e0, qs) else (Nothing, q)
>   returnA -< ret





============================================================
======================== Focus Stuff =======================
============================================================

Here we define focus status as the widget at the location of 
the last left mouse click.  Already, this conflicts with the 
"focus" and "nextFocus" fields of the Sys data type.  Therefore, 
it will likely need to be rewritten/adjusted once we solidify 
how focus will work.

> withFocusStatus :: Bool -> UISF (Bool, a) b -> UISF a b
> withFocusStatus startingFocus sf = conjoin $ proc a -> do
>   mc <- getMouseClicks -< ()
>   ctx <- getCTX -< ()
>   rec inFocus' <- init startingFocus -< inFocus
>       let inFocus = maybe inFocus' (isInFocus inFocus' (bounds ctx)) mc
>   unconjoin sf -< (inFocus, a)
>  where
>   isInFocus :: Bool -> Rect -> (Point, Bool, Bool) -> Bool
>   isInFocus _ bbx (pt, True, True) = pt `inside` bbx
>   isInFocus prev bbx (_, _, _) = prev
>   getCTX :: UISF () CTX
>   getCTX = mkUISF f where
>       f _ (c, s, _) = (nullLayout, s, nullAction, [], c)

============================================================
======================= UISF Getters =======================
============================================================

> withCTX :: UISF (CTX, a) b -> UISF a b
> withCTX sf = conjoin $ arr (\a -> ((),a)) >>> first getCTX >>> (unconjoin sf)
>   where getCTX :: UISF () CTX
>         getCTX = mkUISF f where
>           f _ (c, s, _) = (nullLayout, s, nullAction, [], c)

> getEvents :: UISF () Input
> getEvents = mkUISF f where
>   f _ (_, s, e) = (nullLayout, s, nullAction, [], e)

> getMouseClicks :: UISF () (Event (Point, Bool, Bool))
> getMouseClicks = mkUISF f where
>   f _ (_, s, UIEvent (Button pt left down)) = (nullLayout, s, nullAction, [], Just (pt, left, down))
>   f _ (_, s, _) = (nullLayout, s, nullAction, [], Nothing)

> getKeyStrokes :: UISF () (Event (Char, Bool))
> getKeyStrokes = mkUISF f where
>   f _ (_, s, UIEvent (Key c down)) = (nullLayout, s, nullAction, [], Just (c, down))
>   f _ (_, s, _) = (nullLayout, s, nullAction, [], Nothing)

> getMousePosition :: UISF () Point
> getMousePosition = proc _ -> do
>   e <- getEvents -< ()
>   rec p' <- init (0,0) -< p
>       let p = case e of
>                   UIEvent (MouseMove pt) -> pt
>                   _                      -> p'
>   returnA -< p


============================================================
=============== UI colors and drawing routine ==============
============================================================

> bg = rgb 0xec 0xe9 0xd8
> gray0 = rgb 0xff 0xff 0xff
> gray1 = rgb 0xf1 0xef 0xe2
> gray2 = rgb 0xac 0xa8 0x99
> gray3 = rgb 0x71 0x6f 0x64
> blue3 = rgb 0x31 0x3c 0x79

> box [] ((x, y), (w, h)) = nullGraphic 
> box ((t, b):cs) ((x, y), (w, h)) = 
>   box cs ((x + 1, y + 1), (w - 2, h - 2)) // 
>   withColor' t (line (x, y) (x, y + h - 1) //
>                 line (x, y) (x + w - 2, y)) //
>   withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
>                 line (x + w - 1, y) (x + w - 1, y + h - 1))

> block ((x,y), (w, h)) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]
>
> pushed = (gray2, gray0) : (gray3, gray1) : []
> popped = (gray1, gray3) : (gray0, gray2) : []
> marked = (gray2, gray0) : (gray0, gray2) : []

> inside (u, v) ((x, y), (w, h)) = u >= x && v >= y && u < x + w && v < y + h

