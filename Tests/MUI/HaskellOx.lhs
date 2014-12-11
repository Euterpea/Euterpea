> {-# LANGUAGE Arrows, TupleSections #-}
> {-# LANGUAGE DoRec, Arrows #-}

Donya Quick
Last modified: 21-Sept-2014
Version: 2

===================
ABOUT THIS PROGRAM

Code to reproduce some of the functionality of the classic MidiOx 
program while maintaining platform independence. This program does 
not attempt to reproduce all of MidiOx's features, only the basic
ability to tie one group of MIDI input and outputs together.

MIDI events will be gathered from the selected input devices and
sent to the selected output devices. Make sure all input and output
devices are connected before starting the program, otherwise they
will not be detected.

You need to have the following installed:
- one or more MIDI input devices or virtual MIDI ports
- one or more MIDI output devices

If you have no virtual ports or input devices, you will not see 
any listed and this program will be of no use to you.

NOTE: The performance from the Microsoft GM Wavetable Synth is very 
laggy. You can use another synthesizer like SyFonOne with ASIO 
drivers to get much improved performance. There is no way (that I 
know of) to improve the performance of the MS synth.

WARNING: You can create MIDI feedback if you have virtual MIDI ports
installed and check the boxes for the same port in both the input and
output sections. If using Loopbe1, you will get an error message when
feedback is detected and the port will mute.

WARNING: It is undavisable to run this program in GHCI! Use GHC to 
compile the program. If run in GHCI, the interpreter is prone to 
periodically malfunctioning and hanging. On Windows, this strange 
crashing from GHCI can be serious enough to require a reboot. This 
is NOT a problem when the program is compiled to a native executable 
using GHC!  

===================
HOW TO INSTALL

You do not need to install this program to run it, but you do need
to have Haskell Platform and Euterpea installed.

1. Install Haskell Platform. You can obtain it for your OS here:
http://www.haskell.org/platform/

2. Install Euterpea using the instructions here:
http://haskell.cs.yale.edu/euterpea-2/download/

However, you will need the DEVELOPMENT branch of the repository 
on github. To get the development branch, you will need to cd into
your Euterpea repository from an Admin-level command prompt and do
the following:

git checkout -b development
cabal install

This will switch your Euterpea installation to the development 
branch. To switch back to the master version, you can write "master"
instead of "development" in the command series above. To see which 
branch you are currently using, you can cd into your Euterpea copy 
and run:

git branch

The currently selected branch will appear with a star next to it.

3. Open up a command prompt/terminal and run the following from
the directory containing HaskellOx.lhs:
ghc -O2 -o "haskellOx.exe" HaskellOx.lhs
(or using whatever extension your OS expects in place of ".exe")

4. Run the compiled program!

===================
SOURCE CODE


> module Main where

> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions -- previously Control.SF.AuxFunctions
> import Control.Arrow

> import Euterpea.Experimental (runMIDI)


> main = haskellOx

Using check boxes to rout midi output. 

> haskellOx2 = runMUI' $ proc _ -> do
>     mi <- selectInputM  -< ()
>     mo <- selectOutputM -< ()
>     _ <- runMIDI sf -< ((), (mi, mo))
>     returnA -< ()
>   where
>     -- (SF (outputdevicelist, SEvent [MidiMessage]) (c,[(DeviceID, SEvent [MidiMessage])]))
>     sf = arr id

> haskellOx = runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="HaskellOx", uiTickDelay=0}) $ proc _ -> do
>   mi <- selectInputM-< ()
>   m <- midiInM -< mi
>   mo <- selectOutputM -< ()
>   b <- midiOutMB -< mProc mo m 
>   returnA -< () where
>   f = map fst . filter snd

> mProc :: [DeviceID] -> SEvent [MidiMessage] -> [(DeviceID, BufferOperation MidiMessage)]
> mProc outDevs msgs = map (\d -> (d, ztStamp msgs)) outDevs where
>     ztStamp Nothing = NoBOp
>     ztStamp (Just ms) = AppendToBuffer $ map (\m -> (0,m)) ms