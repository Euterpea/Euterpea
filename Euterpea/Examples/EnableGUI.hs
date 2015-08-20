{-# LANGUAGE ForeignFunctionInterface #-}
{--
On Mac OS X, with versions of GHC prior to 7.8, you will have to use this
``EnableGUI trick'' to run GUI programs for Euterpea from within ghci.

To do so, first compile this file, EnableGUI.hs, to binary:
    ghc -c -fffi EnableGUI.hs

(Note: on some systems it is necessary to add the option
``-framework ApplicationServices'')
Then, run your Euterpea GUI programs in ghci like this:

ghci UIExamples.hs EnableGUI
*UIExamples> :m +EnableGUI
*UIExamples EnableGUI> enableGUI >> main

With this, GHCi will be able to fully activate the Graphics Window. (Fully
compiled GUI programs do not suffer from this anomaly.)

--}

module EnableGUI(enableGUI) where

import Data.Int
import Foreign

type ProcessSerialNumber = Int64

foreign import ccall "GetCurrentProcess" getCurrentProcess :: Ptr ProcessSerialNumber -> IO Int16
foreign import ccall "_CGSDefaultConnection" cgsDefaultConnection :: IO ()
foreign import ccall "CPSEnableForegroundOperation" cpsEnableForegroundOperation :: Ptr ProcessSerialNumber -> IO ()
foreign import ccall "CPSSignalAppReady" cpsSignalAppReady :: Ptr ProcessSerialNumber -> IO ()
foreign import ccall "CPSSetFrontProcess" cpsSetFrontProcess :: Ptr ProcessSerialNumber -> IO ()

enableGUI = alloca $ \psn -> do
    getCurrentProcess psn
    cgsDefaultConnection
    cpsEnableForegroundOperation psn
    cpsSignalAppReady psn
    cpsSetFrontProcess psn
