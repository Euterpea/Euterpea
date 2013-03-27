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
> import qualified Graphics.UI.GLFW as SK (SpecialKey (..))

> import Control.SF.AuxFunctions

> import Prelude hiding (init)
> import Control.Arrow
> import Control.CCA.Types

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Char (isPrint, ord)
> import Codec.Midi (Message(..))
> import Euterpea.IO.MIDI.ToMidi
> import Euterpea.IO.MIDI.GeneralMidi
> import Euterpea.Music.Note.Performance hiding (Event)
> import Euterpea.Music.Note.Music
> import Data.List hiding (init)
> import Data.Maybe (isJust)

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
>         (Rect -> Bool -> s -> Graphic) ->         -- drawing routine
>         (s -> Sound) ->                           -- sound routine
>         (a -> s -> s1) ->                         -- input injection
>         ((s1, (CTX, Input)) -> (s2, DirtyBit)) -> -- computation
>         (s2 -> (b, s)) ->                         -- output projection
>         UISF a b
> mkWidget i layout draw buzz inj comp prj = proc a -> do
>   rec s  <- init i -< s'
>       (y, s') <- mkUISF aux -< inj a s
>   returnA -< y
>   --loop $ second (init i) >>> arr (uncurry inj) >>> mkUISF aux
>     where
>       aux s1 (ctx,f,inp) = (layout, db, f, action, nullCD, (y, s'))
>         where
>           (s2, db) = comp (s1, (ctx, inp))
>           (y, s') = prj s2
>           action = (draw bbx (snd f == HasFocus) `cross` buzz) s'
>           bbx = bounds ctx --computeBBX ctx layout

A few useful shorthands for creating widgets with mkWidget

> dup x = (x, x)
> pair = (,)



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
>     aux a (ctx,f,inp) = (d, False, f, action, nullCD, a)
>       where 
>         action = justGraphicAction $ drawit bbx
>         bbx = bounds ctx --computeBBX ctx d

-----------------
 | Display Box | 
-----------------
DisplayStr is an output widget showing the instantaneous value of
a signal of strings.

> displayStr :: UISF String ()
> displayStr = mkWidget "" d draw (const nullSound) pair 
>   (\((v, v'), (_, _)) -> (v, v /= v'))
>   (\s -> ((), s))
>   where
>     minh = 16 + padding * 2
>     d = makeLayout (Stretchy 8) (Fixed minh)
>     draw b@(p@(x,y), (w, h)) _ s = 
>       let n = (w - padding * 2) `div` 8
>       in withColor Black (text (x + padding, y + padding) (take n s)) // 
>          (box pushed b) // (withColor White $ block b) 

display is a widget that takes any show-able value and displays it.

> display :: Show a => UISF a ()
> display = arr show >>> displayStr

withDisplay is a widget modifier that modifies the given widget 
so that it also displays its output value.

> withDisplay :: Show b => UISF a b -> UISF a b
> withDisplay sf = proc a -> do
>   b <- sf -< a
>   display -< b
>   returnA -< b


--------------
 | Text Box | 
--------------
Textbox is a widget showing the instantaneous value of a signal of 
strings.  It takes one static arguments:
    startingVal - The initial value in the textbox

The textbox widget will often be used with ArrowLoop (the rec keyword).  
However, it uses init internally, so there should be no fear of a blackhole.

The textbox widget supports mouse clicks and typing as well as the 
left, right, end, home, delete, and backspace special keys.

> textbox :: String -> UISF String String
> textbox startingVal = focusable $ 
>   conjoin $ proc s -> do
>     inFocus <- isInFocus -< ()
>     k <- getEvents -< ()
>     ctx <- getCTX -< ()
>     rec (s', i) <- init (startingVal, 0) -< if inFocus then update s i ctx k else (s, i)
>     displayStr -< seq i s'
>     t <- time -< ()
>     b <- timer -< (t, 0.5)
>     rec willDraw <- init True -< willDraw'
>         let willDraw' = if isJust b then not willDraw else willDraw
>     canvas' displayLayout drawCursor -< Just (willDraw && inFocus, i)
>     returnA -< s'
>   where
>     minh = 16 + padding * 2
>     displayLayout = makeLayout (Stretchy 8) (Fixed minh)
>     update s i _ (UIEvent (Key c True)) = (take i s ++ [c] ++ drop i s, i+1)
>     update s i _ (UIEvent (SKey SK.BACKSPACE True)) = (take (i-1) s ++ drop i s, max (i-1) 0)
>     update s i _ (UIEvent (SKey SK.DEL       True)) = (take i s ++ drop (i+1) s, i)
>     update s i _ (UIEvent (SKey SK.LEFT      True)) = (s, max (i-1) 0)
>     update s i _ (UIEvent (SKey SK.RIGHT     True)) = (s, min (i+1) (length s))
>     update s i _ (UIEvent (SKey SK.END       True)) = (s, length s)
>     update s i _ (UIEvent (SKey SK.HOME      True)) = (s, 0)
>     update s i ctx (UIEvent (Button (x,_) True True)) = (s, min (length s) $ (x - xoffset ctx) `div` 8)
>     update s i _ _ = (s, max 0 $ min i $ length s)
>     drawCursor (False, _) _ = nullGraphic
>     drawCursor (True, i) (w,h) = 
>         let n = (w - padding * 2) `div` 8
>             linew = padding + i*8
>         in if linew > w then nullGraphic else withColor Black $
>             line (linew, padding) (linew, 16+padding)
>     xoffset = fst . fst . bounds


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
>     modsf sf a (ctx@(CTX _ bbx@((x,y), (w,h)) m _),f,inp) = do
>       (l,db,f',action,ts,(v,nextSF)) <- expandUISF sf a (CTX TopDown ((x + 4, y + 20), (w - 8, h - 32))
>                                                         m False, f, inp)
>       let d = l { hFixed = hFixed l + 8, vFixed = vFixed l + 36, 
>                   minW = max (tw + 20) (minW l), minH = max 36 (minH l) }
>       return (d, db, f', (\(g, s) -> (drawit bbx g, s)) action, ts, (v,compressUISF (modsf nextSF)))


------------
 | Button | 
------------
Button is an input widget with a state of being on or off.  
It also shows a static text label.

> button :: String -> UISF () Bool
> button label = focusable $ 
>   mkWidget False d draw (const nullSound) (const id)
>        process dup
>   where
>     (tw, th) = (8 * length label, 16) 
>     (minw, minh) = (tw + padding * 2, th + padding * 2)
>     d = makeLayout (Stretchy minw) (Fixed minh)
>     draw b@((x,y), (w,h)) inFocus down = 
>       let x' = x + (w - tw) `div` 2 + if down then 0 else -1
>           y' = y + (h - th) `div` 2 + if down then 0 else -1
>       in (withColor Black $ text (x', y') label) // 
>          (if inFocus then box marked b else nullGraphic) //
>          (box (if down then pushed else popped) b)
>     process (s, (ctx, evt)) = (s', s /= s')
>       where 
>         s' = case evt of
>           UIEvent (Button pt True down) -> case (s, down) of
>             (False, True) -> True
>             (True, False) -> False
>             _ -> s
>           UIEvent (MouseMove pt) -> if pt `inside` bbx then s else False
>           UIEvent (SKey SK.ENTER down) -> down
> -- Currently, non-special keys are implemented such that there is only a 
> -- Press event and no Release event.  To work with buttons, we will need 
> -- to add some state or something.
> --          UIEvent (Key ' ' down) -> down
>           _ -> s
>           where
>             bbx = bounds ctx --computeBBX ctx d

-------------------
 | Sticky Button | 
-------------------
The Sticky Button is like the button, but when it is pressed, it remains 
depressed until it is clicked again to be released.  Thus, it looks like a 
button, but it behaves more like a checkbox.

> stickyButton :: String -> UISF () Bool
> stickyButton label = focusable $ 
>   mkWidget False d draw (const nullSound) (const id)
>        process dup
>   where
>     (tw, th) = (8 * length label, 16) 
>     (minw, minh) = (tw + padding * 2, th + padding * 2)
>     d = makeLayout (Stretchy minw) (Fixed minh)
>     draw b@((x,y), (w,h)) inFocus down = 
>       let x' = x + (w - tw) `div` 2 + if down then 0 else -1
>           y' = y + (h - th) `div` 2 + if down then 0 else -1
>       in (withColor Black $ text (x', y') label) // 
>          (if inFocus then box marked b else nullGraphic) //
>          (box (if down then pushed else popped) b)
>     process (s, (ctx, evt)) = (s', s /= s')
>       where 
>         s' = case evt of
>           UIEvent (Button pt True True) -> not s
>           UIEvent (SKey SK.ENTER True) -> not s
>           _ -> s
>           where
>             bbx = bounds ctx


---------------
 | Check Box | 
---------------
Checkbox allows selection or deselection of an item.
It has a static label as well as an initial state.

> checkbox :: String -> Bool -> UISF () Bool
> checkbox label state = proc _ -> do
>   rec s  <- init state -< s'
>       e  <- edge <<< toggle state d draw -< s
>       let s' = maybe s (const $ not s) e
>   returnA -< s'
>   where
>     (tw, th) = (8 * length label, 16) 
>     (minw, minh) = (tw + padding * 2, th + padding * 2)
>     d = makeLayout (Stretchy minw) (Fixed minh)
>     draw ((x,y), (w,h)) inFocus down = 
>       let x' = x + padding + 16 
>           y' = y + (h - th) `div` 2
>           b = ((x + padding + 2, y + h `div` 2 - 6), (12, 12))
>       in (withColor Black $ text (x', y') label) // 
>          (if inFocus then box marked b else nullGraphic) //
>          (if down 
>             then withColor' gray3 (polyline 
>               [(x + padding + 5, y + h `div` 2),
>                (x + padding + 7, y + h `div` 2 + 3),
>                (x + padding + 11, y + h `div` 2 - 2)])
>             else nullGraphic) //
>       box pushed b // (withColor White $ block b)


--------------------
 | Checkbox Group | 
--------------------

The checkGroup widget creates a group of check boxes that
all send to the same output stream. It takes a list of 
labels for the check boxes.

> checkGroup :: [String] -> UISF () [Bool]
> checkGroup (s:ss) = proc _ -> do
>     c <- checkbox s False -< ()
>     cs <- checkGroup ss -< ()
>     returnA -< (c:cs)
> checkGroup [] = proc _ -> do
>     returnA -< []



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
>     aux :: Int -> [String] -> UISF Int (SEvent Int)
>     aux j [] = arr (const Nothing)
>     aux j (l:labels) = proc n -> do
>       u <- edge <<< toggle (i == j) d draw -< n == j
>       v <- aux (j + 1) labels -< n
>       returnA -< maybe v (const $ Just j) u
>       where
>         (tw, th) = (8 * length l, 16) 
>         (minw, minh) = (tw + padding * 2, th + padding * 2)
>         d = makeLayout (Stretchy minw) (Fixed minh)
>         draw ((x,y), (w,h)) inFocus down = 
>           let x' = x + padding + 16 
>               y' = y + (h - th) `div` 2
>           in (withColor Black $ text (x', y') l) // 
>              (if down then circle gray3 (x,y) (5,6) (9,10) else nullGraphic) //
>              (circle gray3 (x,y) (2,3) (12,13)) //
>              (circle gray0 (x,y) (2,3) (13,14)) //
>              (if inFocus then circle gray2 (x,y) (0,0) (14,15) else nullGraphic)


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
at once, we use [] rather than SEvent for the type.
The values in the (value,time) event pairs should be between -1 and 1.

The below two implementation of realtimeGraph produce the same output, 
but the first one performs better for some reason.  I'm not sure why ...

> realtimeGraph :: RealFrac a => Layout -> Time -> Color -> UISF (Time, [(a,Time)]) ()
> realtimeGraph layout hist color = 
>   mkWidget ([(0,0)],0) layout draw (const nullSound) (,) process (\s -> ((), s))
>   where draw ((x,y), (w, h)) _ (lst,t) = translateGraphic (x,y) $ 
>           if null lst then nullGraphic else withColor color $ polyline (map (adjust t) lst)
>           where adjust t (i,t0) = (truncate $ fromIntegral w * (hist + t0 - t) / hist,
>                                    buffer + truncate (fromIntegral (h - 2*buffer) * (1 + i)/2))
>                 buffer = truncate $ fromIntegral h / 10
>         removeOld _ [] = []
>         removeOld t ((i,t0):is) = if t0+hist>=t then (i,t0):is else removeOld t is
>         process (((t,is),(lst,_)), _) = ((removeOld t (lst ++ is), t), True)

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

> histogram :: RealFrac a => Layout -> UISF (SEvent [a]) ()
> histogram layout = 
>   mkWidget Nothing layout draw (const nullSound) inj process (\s -> ((), s))
>   where inj inp prev = maybe prev Just inp
>         process (Nothing, _) = (Nothing, False)
>         process (Just a,  _) = (Just a, True)
>         draw ((x,y), (w, h)) _ = translateGraphic (x,y) . mymap (polyline . mkPts)
>           where mkPts l  = zip (xs $ length l) (map adjust . normalize . reverse $ l)
>                 xs n     = reverse $ map truncate [0,(fromIntegral w / fromIntegral (n-1))..(fromIntegral w)]
>                 adjust i = buffer + truncate (fromIntegral (h - 2*buffer) * (1 - i))
>                 normalize lst = map (/m) lst where m = maximum lst
>                 buffer = truncate $ fromIntegral h / 10
>                 mymap :: ([a] -> Graphic) -> SEvent [a] -> Graphic
>                 mymap f (Just lst@(_:_)) = f lst
>                 mymap _ _ = nullGraphic

 histogram :: RealFrac a => Layout -> UISF (SEvent [a]) ()
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

--------------
 | List Box | 
--------------
The listbox widget creates a box with selectable entries.
The input stream is the list of entries as well as which entry is 
currently selected, and the output stream is the index of the newly 
selected entry.  Note that the index can be greater than the length 
of the list (simply indicating no choice selected).

> listbox :: (Eq a, Show a) => UISF ([a], Int) Int
> listbox = focusable $ init (-1) <<< mkWidget 
>   ([], -1) layout draw (const nullSound) pair
>   process (\(lst,i) -> (i, (lst,i)))
>   where
>     layout = makeLayout (Stretchy 80) (Stretchy 16)
>     -- takes the rectangle to draw in and a tuple of the list of choices and the index selected
>     lineheight = 16
>     --draw :: Show a => Rect -> ([a], Int) -> Graphic
>     draw rect@((x,y),(w,h)) _ (lst, i) = 
>         genTextGraphic rect i lst // 
>           (box pushed rect) // (withColor White $ block rect)
>         where
>           n = (w - padding * 2) `div` 8
>           genTextGraphic _ _ [] = nullGraphic
>           genTextGraphic ((x,y),(w,h)) i (v:vs) = (if i == 0
>                 then withColor White (text (x + padding, y + padding) (take n (show v))) //
>                      withColor Blue (block ((x,y),(w,lineheight)))
>                 else withColor Black (text (x + padding, y + padding) (take n (show v)))) //
>             genTextGraphic ((x,y+lineheight),(w,h-lineheight)) (i - 1) vs
>     process (((lst,i), olds), (ctx, inp)) = 
>       ((lst,i'), olds == (lst,i'))
>         where
>         i' = case inp of
>           UIEvent (Button pt True True) -> pt2index pt
>           UIEvent (SKey SK.DOWN True)   -> min (i+1) (length lst - 1)
>           UIEvent (SKey SK.UP   True)   -> max (i-1) 0
>           _ -> i
>         ((_,y),_) = bounds ctx
>         pt2index (px,py) = (py-y) `div` lineheight


-------------------
 | Midi Controls | 
-------------------
midiIn is a widget that accepts a MIDI device ID and returns the event 
stream of MidiMessages that that device is producing.

midiOut is a widget that accepts a MIDI device ID as well as a stream 
of MidiMessages and sends the MidiMessages to the device.

> midiIn :: UISF DeviceID (SEvent [MidiMessage])
> midiIn = mkUISF aux 
>   where 
>     aux dev (ctx,foc,inp) = (nullLayout, False, foc, action, nullCD, v)
>       where 
>         v = case inp of
>               MidiEvent d m | d == dev -> Just [Std m]
>               _ -> Nothing
>         action = justSoundAction $ do
>           valid <- isValidInputDevice dev
>           when valid $ pollMidi dev (cb dev)
>         cb d (t, m) = inject ctx (MidiEvent d m)
 
> midiOut :: UISF (DeviceID, SEvent [MidiMessage]) ()
> midiOut = mkUISF aux 
>   where
>     aux (dev, mmsg) (_,foc,_) = (nullLayout, False, foc, action, nullCD, ())
>       where
>         action = justSoundAction $ do
>           valid <- isValidOutputDevice dev 
>           when valid $ case mmsg of
>                 Just msgs -> tryOutputMidi dev >>
>                              mapM_ (\m -> outputMidi dev (0, m)) msgs
>                 Nothing   -> tryOutputMidi dev

 
The midiInM widget takes input from multiple devices and combines 
it into a single stream. 

> midiInM :: UISF ([(DeviceID, Bool)]) (SEvent [MidiMessage])
> midiInM = proc ds -> do
>   midiInGroup (map fst devs) -< ds
>       where devs = filter (\(i,d) -> input d && name d /= "Microsoft MIDI Mapper") $ 
>                      zip [0..] $ unsafePerformIO getAllDeviceInfo

> midiInGroup :: [DeviceID] -> UISF ([(DeviceID, Bool)]) (SEvent [MidiMessage])
> midiInGroup [] = proc bs -> do
>     returnA -< Nothing
> midiInGroup (i:is) = proc bs -> do 
>     m <- midiIn -< i
>     let m' = if elem (i, True) bs then m else Nothing
>     ms <- midiInGroup is -< bs
>     returnA -< merge m' ms where
>     merge :: Maybe [a] -> Maybe [a] -> Maybe [a]
>     merge Nothing (Just x) = Just x
>     merge (Just x) Nothing = Just x
>     merge (Just x) (Just y) = Just (x++y)
>     merge Nothing Nothing = Nothing


A midiOutM widget sends output to multiple MIDI devices by sequencing
the events through a single midiOut. The same messages are sent to 
each device. The midiOutM is designed to be hooked up to a stream like
that from a checkGroup.

> midiOutM :: UISF ([(DeviceID, Bool)], SEvent [MidiMessage]) ()
> midiOutM = proc (ds, ms) -> do
>   midiOutGroup (map fst devs) -< (ds, ms)
>       where devs = filter (\(i,d) -> output d && name d /= "Microsoft MIDI Mapper") $ 
>                      zip [0..] $ unsafePerformIO getAllDeviceInfo 

> midiOutGroup :: [Int] -> UISF ([(DeviceID, Bool)], SEvent [MidiMessage]) ()
> midiOutGroup [] = proc _ -> do
>     returnA -< ()
> midiOutGroup (i:is) = proc (bs, ms) -> do 
>     midiOut -< (i, if elem (i, True) bs then ms else Nothing)
>     midiOutGroup is -< (bs, ms)


A midiOutB widget wraps the regular midiOut widget with a buffer. 
This allows for a timed series of messages to be prepared and sent
to the widget at one time. With the regular midiOut, there is no
timestamping of the messages and they are assumed to be played "now"
rather than at some point in the future. Just as MIDI files have the
events timed based on ticks since the last event, the events here 
are timed based on seconds since the last event. If an event is 
to occur 0.0 seconds after the last event, then it is assumed to be
played at the same time as that other event and all simultaneous 
events are handed to midiOut at the same timestep. Finally, the 
widget returns a flat that is True if the buffer is empty and False
if the buffer is full (meaning that items are still being played).

> midiOutB :: UISF (DeviceID, SEvent [(Time, MidiMessage)]) (Bool)
> midiOutB = proc (devID, msgs) -> do
>   t <- time -< ()
>   rec tLast <- init 0.0 -< nextT
>       msgBuffer <- init [] -< msgBuffer''
>       let (nextMsgs, msgBuffer', nextT) = getNextMsgs tLast t msgBuffer
>           msgBuffer'' = case msgs of Just ms -> msgBuffer' ++ ms
>                                      Nothing -> msgBuffer'
>   midiOut -< (devID, nextMsgs) 
>   returnA -< null msgBuffer where 
>       getNextMsgs :: Time -> Time -> [(Time, a)] -> (Maybe [a], [(Time, a)], Time)
>       getNextMsgs tLast t ms = 
>           let (x:xs) = head ms : (takeWhile ((<=0).fst) $ tail ms)
>               nextMs = map snd (x:xs)
>               tNext = fst x
>           in  if null ms || t - tLast < fst x then (Nothing, ms, tLast)
>               else (Just nextMs, drop (length (x:xs)) ms, t)


The musicToMsgs function bridges the gap between a Music1 value and
the input type of midiOutB. It turns a Music1 value into a series 
of MidiMessages that are timestamped using the number of seconds 
since the last event. The arguments are as follows:

- True if allowing for an infinite music value, False if the input
  value is known to be finite. 

- InstrumentName overrides for channels for infinite case. When the
  input is finite, an empty list can be supplied since the instruments
  will be pulled from the Music1 value directly (which is obviously 
  not possible to do in the infinite case).

- The Music1 value to convert to timestamped MIDI messages.

> musicToMsgs :: Bool -> [InstrumentName] -> Music1 -> [(Time, MidiMessage)]
> musicToMsgs inf is m = 
>     let p = perform defPMap defCon m -- obtain the performance
>         instrs = if null is && not inf then nub $ map eInst p else is
>         chan e = 1 + case findIndex (==eInst e) instrs of 
>                          Just i -> i
>                          Nothing -> error ("Instrument "++show (eInst e)++
>                                     "is not assigned to a channel.")                               
>         f e = (eTime e, ANote (chan e) (ePitch e) (eVol e) (fromRational $ eDur e))
>         f2 e = [(eTime e, Std (NoteOn (chan e) (ePitch e) (eVol e))), 
>                (eTime e + eDur e, Std (NoteOff (chan e) (ePitch e) (eVol e)))]
>         evs = if inf then map f p else sortBy mOrder $ concatMap f2 p -- convert to MidiMessages
>         times = map (fromRational.fst) evs -- absolute times
>         newTimes = zipWith subtract (head times : times) times -- relative times
>         progChanges = zipWith (\c i -> (0, Std $ ProgramChange c i)) 
>                       [1..16] $ map toGM instrs
>     in  if length instrs > 16 then error "too many instruments!" 
>         else progChanges ++ zip newTimes (map snd evs) where
>     mOrder (t1,m1) (t2,m2) = compare t1 t2

 
 
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
>   returnA -< devId
>       where devs = filter (\(i,d) -> f d && name d /= "Microsoft MIDI Mapper") $ 
>                      zip [0..] $ unsafePerformIO getAllDeviceInfo
>             defaultChoice = if null devs then (-1) else 0


The selectInputM and selectOutputM widgets use checkboxes instead of 
radio buttons to allow the user to select multiple inputs and outputs.
These widgets should be used with midiInM and midiOutM respectively.

> selectInputM, selectOutputM :: UISF () [(DeviceID, Bool)]
> selectInputM = selectDevM "Input devices" input
> selectOutputM = selectDevM "Output devices" output

> selectDevM :: String -> (DeviceInfo -> Bool) -> UISF () [(DeviceID, Bool)]
> selectDevM t f = 
>   let devNames = snd $ unzip devs  -- names
>       devIDs = map fst devs -- ids
>   in  title t $ proc _ -> do
>       cs <- checkGroup (map name $ devNames) -< ()
>       returnA -< zip devIDs cs
>         where devs = filter (\(i,d) -> f d && name d /= "Microsoft MIDI Mapper") $ 
>                        zip [0..] $ unsafePerformIO getAllDeviceInfo



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

> toggle :: (Eq s) => s                     -- Initial state value
>        -> Layout                          -- The layout for the toggle
>        -> (Rect -> Bool -> s -> Graphic)  -- The drawing routine
>        -> UISF s Bool
> toggle init layout draw = focusable $ 
>   mkWidget init layout draw (const nullSound) pair process id
>   where
>     process ((s,s'), (ctx, evt)) = ((on,s), s /= s')
>       where 
>         on = case evt of
>           UIEvent (Button pt True down) -> down
>           UIEvent (SKey SK.ENTER down) -> down
>           UIEvent (Key ' ' down) -> down
>           _ -> False 

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
> mkSlider hori val2pos pos2val jump val0 = focusable $ 
>   mkWidget (val0, Nothing) d draw (const nullSound) (const id)
>        process (\s -> (fst s, s))
>   where
>     rotP p@(x,y) ((bx,by),_) = if hori then p else (bx + y - by, by + x - bx)
>     rotR r@(p@(x,y),(w,h)) bbx = if hori then r else (rotP p bbx, (h,w))
>     (minw, minh) = (16 + padding * 2, 16 + padding * 2)
>     (tw, th) = (16, 8)
>     d = if hori then makeLayout (Stretchy minw) (Fixed minh)
>                 else makeLayout (Fixed minh) (Stretchy minw)
>     val2pt val ((bx,by), (bw,bh)) = 
>       let p = val2pos val (bw - padding * 2 - tw)
>       in (bx + p + padding, by + 8 - th `div` 2 + padding) 
>     bar ((x,y),(w,h)) = ((x + padding + tw `div` 2, y + 6 + padding), 
>                          (w - tw - padding * 2, 4))
>     draw b@((x,y), (w, h)) inFocus (val, _) = 
>       let p@(mx,my) = val2pt val (rotR b b)
>       in box popped (rotR (p, (tw, th)) b)
>          // (if inFocus then box marked (rotR (p, (tw-2, th-2)) b) else nullGraphic)
>          // (withColor' bg $ block $ rotR ((mx + 2, my + 2), (tw - 4, th - 4)) b)
>          // (box pushed $ rotR (bar (rotR b b)) b)
>     process ((val, s), (ctx, evt)) = ((val', s'), val /= val')
>       where
>         (val', s') = case evt of
>           UIEvent (Button pt' True down) -> let pt = rotP pt' bbx in
>             case (pt `inside` target, down) of
>               (True, True) -> (val, Just (ptDiff pt val))
>               (_, False)   -> (val, Nothing)
>               (False, True) | pt `inside` bar' -> clickonbar pt
>               _ -> (val, s)
>           UIEvent (MouseMove pt') -> let pt = rotP pt' bbx in
>             case s of
>               Just pd -> (pt2val pd pt, Just pd)
>               Nothing -> (val, s)
>           UIEvent (SKey SK.LEFT  True) -> if hori then (jump (-1) bw val, s) else (val, s)
>           UIEvent (SKey SK.RIGHT True) -> if hori then (jump 1    bw val, s) else (val, s)
>           UIEvent (SKey SK.UP    True) -> if hori then (val, s) else (jump (-1) bw val, s)
>           UIEvent (SKey SK.DOWN  True) -> if hori then (val, s) else (jump 1    bw val, s)
>           UIEvent (SKey SK.HOME  True) -> (pos2val 0  (bw - 2 * padding - tw), s)
>           UIEvent (SKey SK.END   True) -> (pos2val bw (bw - 2 * padding - tw), s)
>           _ -> (val, s)
>         bbx@((bx,by),(bw,bh)) = let b = bounds ctx {-computeBBX ctx d-} in rotR b b
>         bar' = let ((x,y),(w,h)) = bar bbx in ((x,y-4),(w,h+8))
>         target = (val2pt val bbx, (tw, th)) 
>         ptDiff (x, y) val = 
>           let (x', y') = val2pt val bbx
>           in (x' + tw `div` 2 - x, y' + th `div` 2 - x)
>         pt2val (dx, dy) (x,y) = pos2val (x + dx - bx - tw `div` 2) (bw - 2 * padding - tw)
>         clickonbar pt@(x',y') = 
>           let (x,y) = val2pt val bbx
>               val' = jump (if x' < x then -1 else 1) bw val
>           in (val', s)

------------
 | Canvas | 
------------
Canvas displays any graphics. The input is a signal of graphics
event because we only want to redraw the screen when the input
is there.

> canvas :: Dimension -> UISF (SEvent Graphic) ()
> canvas (w, h) = mkWidget nullGraphic layout draw (const nullSound)
>                 pair process (\s -> ((), s)) 
>   where
>     layout = makeLayout (Fixed w) (Fixed h)
>     draw ((x,y),(w,h)) _ = translateGraphic (x,y)
>     process ((u, g), _) = case u of
>       Just g' -> (g', True)
>       Nothing -> (g,  False)

canvas' uses a layout and a graphic generator which allows canvas to be 
used in cases with stretchy layouts.

> canvas' :: Layout -> (a -> Dimension -> Graphic) -> UISF (SEvent a) ()
> canvas' layout draw = mkWidget Nothing layout drawit (const nullSound)
>                       pair process (\s -> ((), s))
>   where
>     drawit (pt, dim) _ ea = maybe nullGraphic (\a -> translateGraphic pt $ draw a dim) ea
>     process ((ea, a'), _) = case ea of
>       Just a  -> (Just a, True)
>       Nothing -> (a',     False)


============================================================
======================== Focus Stuff =======================
============================================================

Any widget that wants to accept mouse button clicks or keystrokes 
must be focusable.  The focusable function below achieves this.

Making a widget focusable makes it accessible to tabbing and allows 
it to see any mouse button clicks and keystrokes when it is actually 
in focus.

focusable :: UISF a b -> UISF a b
focusable = id

> focusable :: UISF a b -> UISF a b
> focusable widget = proc x -> do
>   rec hasFocus <- init False -< hasFocus'
>       (y, hasFocus') <- compressUISF (h widget) -< (x, hasFocus)
>   returnA -< y
>  where
>   h w (a, hasFocus) (ctx, (myid,focus), inp) = do
>     lshift <- isKeyPressed SK.LSHIFT
>     rshift <- isKeyPressed SK.RSHIFT
>     let isShift = lshift || rshift
>         (f, hasFocus') = case (focus, hasFocus, inp) of
>           (HasFocus, _, _) -> (HasFocus, True)
>           (SetFocusTo n, _, _) | n == myid -> (NoFocus, True)
>           (_, _,    UIEvent (Button pt _ True)) -> (NoFocus, pt `inside` bounds ctx) --computeBBX ctx l
>           (_, True, UIEvent (SKey SK.TAB True)) -> if isShift then (SetFocusTo (myid-1), False) 
>                                                               else (SetFocusTo (myid+1), False)
>           (_, _, _) -> (focus, hasFocus)
>         focus' = if hasFocus' then HasFocus else NoFocus
>         inp' = if hasFocus' then (case inp of 
>               UIEvent (SKey SK.TAB True)-> NoEvent
>               _ -> inp)
>                else (case inp of 
>               UIEvent (Button pt _ True) -> NoEvent
>               UIEvent (Key  _ _) -> NoEvent
>               UIEvent (SKey _ _) -> NoEvent
>               _ -> inp)
>         redraw = hasFocus /= hasFocus'
>     (l, db, _, act, tids, (b, w')) <- expandUISF w a (ctx, (myid,focus'), inp')
>     return $! (l, db || redraw, (myid+1,f), act, tids, ((b, hasFocus'), compressUISF (h w')))

Although mouse button clicks and keystrokes will be available once a 
widget marks itself as focusable, the widget may also simply want to 
know whether it is currently in focus to change its appearance.  This 
can be achieved with the following signal function.

> isInFocus :: UISF () Bool
> isInFocus = mkUISF (\_ (_, foc, _) -> (nullLayout, False, foc, nullAction, nullCD, snd foc == HasFocus))


============================================================
======================= UISF Getters =======================
============================================================

> getCTX :: UISF () CTX
> getCTX = mkUISF f where
>   f _ (c, foc, _) = (nullLayout, False, foc, nullAction, nullCD, c)

> getEvents :: UISF () Input
> getEvents = mkUISF f where
>   f _ (_, foc, e) = (nullLayout, False, foc, nullAction, nullCD, e)

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

> circle c (x, y) (w1, h1) (w2, h2) = 
>   withColor' c $ arc (x + padding + w1, y + padding + h1) 
>                      (x + padding + w2, y + padding + h2) 0 360


> block ((x,y), (w, h)) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]
>
> pushed = (gray2, gray0) : (gray3, gray1) : []
> popped = (gray1, gray3) : (gray0, gray2) : []
> marked = (gray2, gray0) : (gray0, gray2) : []

> inside (u, v) ((x, y), (w, h)) = u >= x && v >= y && u < x + w && v < y + h

