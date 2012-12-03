module Euterpea.IO.MUI 
  ( -- UI functions
    UISF 
  , convertToUISF       -- :: NFData b => Integer -> Int -> SF a b -> UISF a ([b], Bool)
  , Dimension, Rect
  , topDown, bottomUp, leftRight, rightLeft    -- :: UISF a b -> UISF a b
  , setSize             -- :: Dimension -> :: UISF a b -> UISF a b
  , pad                 -- :: (Int, Int, Int, Int) -> :: UISF a b -> UISF a b
  , runUI               -- :: String -> UISF () () -> IO ()
  , runUIEx             -- :: Dimension -> String -> UISF () () -> IO ()
  , time                -- :: UISF () Time
    -- Widgets
  , label               -- :: String -> UISF a a
  , display             -- :: UISF String ()
  , display'            -- :: Show a => UISF a ()
  , withDisplay         -- :: Show b => UISF a b -> UISF a b
  , textbox             -- :: String -> UISF String String
  , title               -- :: String -> UISF a b -> UISF a b
  , button              -- :: String -> UISF () Bool
  , checkbox            -- :: String -> Bool -> UISF () Bool
  , radio               -- :: [String] -> Int -> UISF () Int
  , hSlider, vSlider    -- :: RealFrac a => (a, a) -> a -> UISF () a
  , hiSlider, viSlider  -- :: Integral a => a -> (a, a) -> a -> UISF () a
  , realtimeGraph       -- :: RealFrac a => Layout -> Time -> Color -> UISF (Time, [(a,Time)]) ()
  , histogram           -- :: RealFrac a => Layout -> UISF (Event [a]) ()
  , midiIn              -- :: UISF DeviceID (Event [MidiMessage])
  , midiOut             -- :: UISF (DeviceID, Event [MidiMessage]) ()
  , selectInput, selectOutput    -- :: UISF () DeviceID
  , canvas              -- :: Dimension -> UISF (Event Graphic) ()
  , canvas'             -- :: Layout -> (a -> Dimension -> Graphic) -> UISF (Event a) ()
  , timer               -- :: ArrowInit a => a (Time, Double) Bool
  , delay               -- :: ArrowInit a => Double -> a (Time, Event b) (Event b)
  , delayt              -- :: ArrowInit a => a (Time, Double, Event b) (Event b)
  ) where

import Euterpea.IO.MUI.UIMonad
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.Widget
