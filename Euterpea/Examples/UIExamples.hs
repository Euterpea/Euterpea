{-# LANGUAGE Arrows #-}

import Prelude hiding (init)
import Control.Arrow
import Control.CCA.Types
import Control.SF.SF
import Control.SF.AuxFunctions (edge, fftA, Event)

import Euterpea hiding (Event)
import Euterpea.IO.MUI
import Euterpea.IO.MUI.SOE (withColor', rgb, polygon, Color(..))
import Euterpea.IO.Audio.BasicSigFuns (osc, tableSinesN)
import Euterpea.IO.Audio.Types (rate, SigFun, CtrRate, AudRate)

import Numeric (showHex)
import Data.Maybe (listToMaybe, catMaybes)

fftEx :: UISF () ()
fftEx = proc _ -> do
    f1 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 1: " >>> display') -< f1
    f2 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 2: " >>> display') -< f2
    (d,suc) <- convertToUISF 100 myPureSignal -< (f1, f2)
    let (s,fftData) = unzip d
--    _ <- display' -< suc
--    _ <- display' -< length s
    _ <- histogram (500,150) 20 -< listToMaybe $ catMaybes fftData
    _ <- realtimeGraph' (500,150) 200 20 Black -< s
    returnA -< ()
  where
    squareTable = tableLinearN 2048 0 [(1024,0),(1,1),(1023,1)]
    myPureSignal :: SigFun CtrRate (Double, Double) (Double, Event [Double])
    myPureSignal = proc (f1, f2) -> do
        s1 <- osc (tableSinesN 4096 [1]) 0 -< f1
        s2 <- osc (tableSinesN 4096 [1]) 0 -< f2
        let s = (s1 + s2)/2
        fftData <- fftA 100 256 -< s
        returnA -< (s, fftData)
    
t0 = runUIEx (500,600) "fft Test" fftEx



timeEx = title "Time" $ time >>> display'

buttonEx = title "Buttons" $ proc _ -> do
  (x,y) <- leftRight (proc _ -> do
    x <- edge <<< button "+" -< ()
    y <- edge <<< button "-" -< ()
    returnA -< (x, y)) -< ()
  rec v <- init 0 -< (case (x,y) of
            (True, False) -> v+1
            (False, True) -> v-1
            _ -> v)
  display' -< v

t1 = runUI "a" $ leftRight $ timeEx >>> buttonEx

checkboxEx = title "Checkboxes" $ proc _ -> do
  x <- checkbox "Monday" False -< ()
  y <- checkbox "Tuesday" True -< ()
  z <- checkbox "Wednesday" True -< ()
  let v = bin x ++ bin y ++ bin z
  display -< v
  where
    bin True = "1"
    bin False = "0"

radioButtonEx = title "Radio Buttons" $ radio list 0 >>> arr (list!!) >>> display
  where
    list = ["apple", "orange", "banana"]

shoppinglist = title "Shopping List" $ proc _ -> do
  a <- title "apples"  $ hiSlider 1 (0,10) 3 -< ()
  b <- title "bananas" $ hiSlider 1 (0,10) 7 -< () 
  title "total" $ display' -< (a + b)

colorDemo = setSize (300, 220) $ title "Color" $ pad (4,0,4,0) $ leftRight $ proc _ -> do
  r <- newColorSlider "R" -< ()
  g <- newColorSlider "G" -< ()
  b <- newColorSlider "B" -< ()
  prevRGB <- init (0,0,0) -< (r,g,b)
  changed <- init True    -< (r,g,b) == prevRGB
  let rect = withColor' (rgb r g b) (box ((0,0),d))
  pad (4,8,0,0) $ canvas d -< if changed then Just rect else Nothing
  where
    d = (170,170)
    newColorSlider l = title l $ topDown $ proc _ -> do
      v <- viSlider 16 (0,255) 0 -< ()
      _ <- display -< showHex v ""
      returnA -< v
    box ((x,y), (w, h)) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

main = runUIEx (500,500) "UI Demo" $ 
  (leftRight $ (bottomUp $ timeEx >>> buttonEx) >>> checkboxEx >>> radioButtonEx) >>>
  (leftRight $ shoppinglist >>> colorDemo)

