{-# LANGUAGE Arrows #-}

-- Last modified by: Daniel Winograd-Cort
-- Last modified on: 12/15/2012

-- This file is a set of various UI examples showing off the features 
-- of the various widgets in Euterpea.IO.MUI.

module Euterpea.Examples.UIExamples where

import Euterpea
import Euterpea.Experimental (fftA)
import Euterpea.IO.MUI.SOE (withColor', rgb, polygon)

import Numeric (showHex)
import Data.Maybe (listToMaybe, catMaybes)

-- This example shows off the histogram and realtimeGraph widgets by 
-- summing two sin waves and displaying them.  Additionally, it makes 
-- use of two horizontal sliders.
-- This example also shows off convertToUISF and how to take a SigFun, 
-- of the type used to create sound, and convert it to a UISF.
fftEx :: UISF () ()
fftEx = proc _ -> do
    f1 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 1: " >>> display) -< f1
    f2 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 2: " >>> display) -< f2
    d <- convertToUISF 0.1 myPureSignal -< (f1, f2)
    let fft = listToMaybe $ catMaybes $ map (snd . fst) d
        s = map (\((s, _), t) -> (s,t)) d
    _ <- histogram (makeLayout (Stretchy 10) (Fixed 150)) -< fft
    _ <- realtimeGraph (makeLayout (Stretchy 10) (Fixed 150)) 2 Black -< s
    outA -< ()
  where
    squareTable = tableLinearN 2048 0 [(1024,0),(1,1),(1023,1)]
    myPureSignal :: SigFun CtrRate (Double, Double) (Double, SEvent [Double])
    myPureSignal = proc (f1, f2) -> do
        s1 <- osc (tableSinesN 4096 [1]) 0 -< f1
        s2 <- osc (tableSinesN 4096 [1]) 0 -< f2
        let s = (s1 + s2)/2
        fftData <- fftA 100 256 -< s
        returnA -< (s, fftData)

-- This test is run separately from the others.
t0 :: IO ()
t0 = runUIEx (500,600) "fft Test" fftEx


-- This example displays the time from the start of the GUI application.
timeEx :: UISF () ()
timeEx = title "Time" $ getTime >>> display

-- This example shows off buttons and state by presenting a plus and 
-- minus button with a counter that is adjusted by them.
buttonEx :: UISF () ()
buttonEx = title "Buttons" $ proc _ -> do
  (x,y) <- leftRight (proc _ -> do
    x <- edge <<< button "+" -< ()
    y <- edge <<< button "-" -< ()
    returnA -< (x, y)) -< ()
  rec v <- delay 0 -< (case (x,y) of
            (Just _, Nothing) -> v+1
            (Nothing, Just _) -> v-1
            _ -> v)
  display -< v

-- This example shows off the checkbox widgets.
checkboxEx :: UISF () ()
checkboxEx = title "Checkboxes" $ proc _ -> do
  x <- checkbox "Monday" False -< ()
  y <- checkbox "Tuesday" True -< ()
  z <- checkbox "Wednesday" True -< ()
  let v = bin x ++ bin y ++ bin z
  displayStr -< v
  where
    bin True = "1"
    bin False = "0"

-- This example shows off the radio button widget.
radioButtonEx :: UISF () ()
radioButtonEx = title "Radio Buttons" $ radio list 0 >>> arr (list!!) >>> displayStr
  where
    list = ["apple", "orange", "banana"]

-- This example shows off integral sliders (horizontal in the case).
shoppinglist :: UISF () ()
shoppinglist = title "Shopping List" $ proc _ -> do
  a <- title "apples"  $ hiSlider 1 (0,10) 3 -< ()
  b <- title "bananas" $ hiSlider 1 (0,10) 7 -< () 
  title "total" $ display -< (a + b)

-- This example shows off both vertical sliders as well as the canvas 
-- widget.  The canvas widget can be used to easily create custom graphics 
-- in the GUI.  Here, it is used to make a color swatch that is 
-- controllable with RGB values by the sliders.
colorDemo :: UISF () ()
colorDemo = setSize (300, 220) $ title "Color" $ pad (4,0,4,0) $ leftRight $ proc _ -> do
  r <- newColorSlider "R" -< ()
  g <- newColorSlider "G" -< ()
  b <- newColorSlider "B" -< ()
  prevRGB <- delay (0,0,0) -< (r,g,b)
  changed <- delay True    -< (r,g,b) == prevRGB
  let rect = withColor' (rgb r g b) (box ((0,0),d))
  pad (4,8,0,0) $ canvas d -< if changed then Just rect else Nothing
  where
    d = (170,170)
    newColorSlider l = title l $ topDown $ proc _ -> do
      v <- viSlider 16 (0,255) 0 -< ()
      _ <- displayStr -< showHex v ""
      returnA -< v
    box ((x,y), (w, h)) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- This example shows off the textbox widget.  Text can be typed in, and 
-- that text is transferred to the display widget below when the button 
-- is pressed.
textboxdemo :: UISF () ()
textboxdemo = proc _ -> do
  str <- leftRight $ label "Text: " >>> textboxE "" -< Nothing
  b <- button "Save text to below" -< ()
  rec str' <- delay "" -< if b then str else str'
  leftRight $ label "Saved value: " >>> displayStr -< str'
  outA -< ()

-- This is the main demo that incorporates all of the other examples 
-- together (except for fftEx).  In addition to demonstrating how 
-- different widgets can connect, it also shows off the tabbing 
-- behavior built in to the GUI.  Pressing tab cycles through focuable 
-- elements, and pressing shift-tab cycles in reverse.
main :: IO ()
main = runUIEx (500,500) "UI Demo" $ 
  (leftRight $ (bottomUp $ timeEx >>> buttonEx) >>> checkboxEx >>> radioButtonEx) >>>
  (leftRight $ shoppinglist >>> colorDemo) >>> textboxdemo

