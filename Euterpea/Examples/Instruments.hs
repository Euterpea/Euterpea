{-# LANGUAGE Arrows #-}
module Euterpea.Examples.Instruments where
import Euterpea
import Euterpea.Experimental
import Control.SF.AuxFunctions ((~++))

-- Here is a demonstration of the guitar and piano widgets available in Euterpea.

instrumentDemo :: IO ()
instrumentDemo = runUIEx (1038,706) "Instrument Demo" $ proc _ -> do
    devId <- selectOutput -< ()
    -- The song player is a standard component that plays back a music value. The string is used
    -- as a display name.
    song <- songPlayer [("Sonata In C", tempo 2 sonataInC), ("Frere Jaques", tempo (2/3) fjfj)] -< ()
    -- The InstrumentData structure holds the settings for each instrument.
    -- If you want to be able to change these settings at runtime, a structure like this is necessary
    -- addNotation presents a checkbox that allows the user to toggle the display of pitch classes
    -- on the instruements. In this example, a single settings variable is used.
    rec settings' <- addNotation -< settings
        settings <- hold defaultInstrumentData -< Just settings'
    -- The guitar UISF takes in a keymap (sixString is the only one available at the moment) and
    -- a midi channel. Any midi messages passed into the guitar will be coerced to that channel.
    guitar1 <- guitar sixString 1 -< (settings, Nothing)
    -- Select instrument takes in a channel and an initial midi instrument, 0 is Grand Piano, 25 is steel guitar.
    -- As the slider is changed, it prepends midi messages to its input to change the instrument on the given
    -- channel.
    outG <- selectInstrument 1 25 -< guitar1
    piano1 <- piano defaultMap0 0 -< (settings, song)
    outP <- selectInstrument 0 0 -< piano1
    -- ~++ attempts to concatenate Maybe Lists, using Nothing as though it were []
    midiOut -< (devId, outG ~++ outP)

-- The exposition of Mozart's Sonata in C, the "Easy Sonata"

sonataInC :: Music Pitch
sonataInC = line [c 5 wn, e 5 hn, g 5 hn, b 4 dhn, c 5 en, d 5 en, c 5 hn, rest hn,
             a 5 wn, g 5 hn, c 6 hn, g 5 hn, f 5 en, g 5 en, e 5 en, f 5 en, e 5 hn, rest hn, 
             a 4 qn, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en,
             g 4 qn, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en,
             f 4 qn, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en,
             e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en,
             e 4 qn, f 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, 
             d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en, e 4 en,
             d 4 qn, e 4 en, f 4 en, g 4 en, a 4 en, b 4 en, cs 5 en,
             d 5 en, a 4 en, b 4 en, cs 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             a 5 en, b 5 en, c 6 en, b 5 en, a 5 en, g 5 en, f 5 en, e 5 en,
             f 5 en, g 5 en, a 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en,
             b 4 qn, g 5 qn, e 5 qn, c 5 qn, d 5 qn, g 5 qn, e 5 qn, c 5 qn,
             d 5 hn, g 5 hn, g 4 hn, rest hn,
             fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en,
             f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en,
             g 5 qn, e 5 qn, c 5 dhn, d 5 en, e 5 en, d 5 qn, c 5 qn,
             c 5 dqn, b 4 en, b 4 hn, rest wn, g 5 qn, e 5 qn, c 5 dhn,
             d 5 en, e 5 en, d 5 qn, c 5 qn, c 5 dqn, b 4 en, b 4 hn, rest wn,
             g 5 en, e 3 en,g 3 en, c 4 en, e 4 en, g 5 en, e 5 en, c 5 en,
             a 4 en, f 3 en, a 3 en, c 4 en, f 4 en, a 4 en, c 5 en, a 4 en,
             f 5 en, d 3 en, f 3 en, b 3 en, d 4 en, f 5 en, d 5 en, b 4 en,
             g 4 en, e 3 en, g 3 en, b 3 en, e 4 en, g 4 en, b 4 en, g 4 en,
             e 5 en, c 4 en, e 4 en, a 4 en, c 5 en, e 5 en, c 5 en, a 4 en,
             f 4 en, d 4 en, f 4 en, a 4 en, d 5 en, f 4 en, a 4 en, f 4 en,
             d 6 en, b 3 en, d 4 en, g 4 en, b 4 en, d 6 en, b 5 en, g 5 en,
             e 5 en, c 4 en, e 4 en, g 4 en, c 5 en, c 6 en, g 5 en, e 5 en,
             d 5 wn, d 5 hn, d 5 hn, a 5 wn, a 5 hn, a 5 hn, g 5 qn, a 5 en,
             b 5 en, c 6 en, d 6 en, e 6 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, c 5 en, d 5 en, c 5 hn, c 5 en, g 4 en,
             c 5 en, e 5 en, g 5 en, e 5 en, c 5 en, e 5 en, f 5 en, d 5 en,
             b 4 en, d 5 en, c 5 hn, c 4 en, g 3 en, c 4 en, e 4 en, g 4 en,
             e 4 en, c 4 en, e 4 en, f 4 en, d 4 en, b 3 en, d 4 en, c 4 hn,
             c 5 hn, c 4 hn]

-- Frere-Jaques using some more Euterpea features

fj0, fj1, fj2, fj3, fj4 :: Music Pitch
fj0 = c 4 qn :+: c 4 qn :+: c 4 qn
fj1 = c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn
fj2 = e 4 qn :+: f 4 qn :+: g 4 hn
fj3 = g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c 4 qn
fj4 = c 4 qn :+: g 3 qn :+: c 4 hn

fj :: Music Pitch
fj  = two fj1 :+: two fj2 :+: two fj3 :+: two fj4
    where two m = m :+: m

fjfj :: Music Pitch
fjfj = Modify (Tempo 4) (Modify (Instrument AcousticGrandPiano) fj)
