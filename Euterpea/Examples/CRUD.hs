{-# LANGUAGE Arrows #-}

module Euterpea.Examples.CRUD where
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

import Euterpea.IO.MUI.Widget


crudEx :: UISF () ()
crudEx = proc _ -> do
    rec s  <- init "" -< s'
        s' <- countedTextbox True -< s
    rec t  <- init "" -< t'
        t' <- textbox False -< t
    b <- button "hi" -< ()
    returnA -< ()



main = runUIEx (500,600) "CRUD Example" crudEx
