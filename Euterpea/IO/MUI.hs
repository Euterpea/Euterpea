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
  , button              -- :: String -> UISF () Bool
  , checkbox            -- :: String -> Bool -> UISF () Bool
  , radio               -- :: [String] -> Int -> UISF () Int
  , title               -- :: String -> UISF a b -> UISF a b
  , hSlider, vSlider    -- :: RealFrac a => (a, a) -> a -> UISF () a
  , hiSlider, viSlider  -- :: Integral a => a -> (a, a) -> a -> UISF () a
  , canvas              -- :: Dimension -> UISF (Event Graphic) ()
  , realtimeGraph       -- :: RealFrac a => Dimension -> Int -> Int -> Color -> UISF a ()
  , realtimeGraph'      -- :: RealFrac a => Dimension -> Int -> Int -> Color -> UISF [a] ()
  , histogram           -- :: RealFrac a => Dimension -> Int -> UISF (Event [a]) ()
  , timer               -- :: ArrowInit a => a (Time, Double) Bool
  , delayt              -- :: ArrowInit a => a (Time, Double, Event b) (Event b)
  , midiIn              -- :: UISF DeviceID (Event [MidiMessage])
  , midiOut             -- :: UISF (DeviceID, Event [MidiMessage]) ()
  , selectInput, selectOutput    -- :: UISF () DeviceID
  ) where

import Euterpea.IO.MUI.UIMonad
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.Widget
