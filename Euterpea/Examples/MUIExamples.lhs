> {-# LANGUAGE Arrows #-}

> module Euterpea.Examples.MUIExamples where

> import Euterpea
> import Data.Maybe (mapMaybe)


=============
Chord builder

Here is a simple program that plays the selected chord when a root
note is entered using a Midi input device.

We define a mapping between chord extensions and their intervals with
respect to the root note.

> chordIntervals = [("Maj", [4,3,5]),
>                   ("Maj7", [4,3,4,1]),
>                   ("Maj9", [2,2,3,4,1]),
>                   ("6", [4,3,2,3]),
>                   ("m", [3,4,5]),
>                   ("m7", [3,4,3,2]),
>                   ("m9", [2,1,4,3,2]),
>                   ("m7b5", [3,3,4,2]),
>                   ("mMaj7", [3,4,4,1]),
>                   ("dim", [3,3,3]),
>                   ("7", [4,3,3,2]),
>                   ("9", [2,2,3,3,2]),
>                   ("7b9", [1,3,3,3,2])]

We display the list of extensions on the screen as radio buttons for
the user to click on.

The toChord function takes in the index of the selected chord extension 
and an input message as the root note, and outputs the notes of
the selected chord based on the root note.  For simplicity, we only
process the head of the message list and ignore everything else.

> toChord :: Int -> [MidiMessage] -> [MidiMessage]
> toChord i (ms@(m:_)) = 
>   case m of 
>     Std (NoteOn c k v) -> f NoteOn c k v
>     Std (NoteOff c k v) -> f NoteOff c k v
>     _ -> ms
>   where f g c k v = map (\k -> Std (g c k v)) 
>                         (scanl (+) k (snd (chordIntervals !! i)))

The UI is arranged in the following way.  On the left side, the list
of input and output devices are displayed top-down. On the right is
the list of chord extensions.  We take the name of each extension from
the chordIntervals list to create the radio buttons.  

When a Midi input event occurs, the input message and the currently
selected index to the list of chords is sent to the toChord function,
and the resulting chord is sent to the output device.

> buildChord = runUIEx (500,500) "Chord Builder" $ leftRight $ proc _ -> do
>   (mi,mo) <- topDown (selectInput &&& selectOutput) -< ()
>   m <- midiIn -< mi
>   i <- topDown $ title "Extension" $ radio (fst (unzip chordIntervals)) 0 -< ()
>   midiOut -< (mo, fmap (toChord i) m)


=================
Bifurcate example

Here is an example with some ideas borrowed from Gary Lee Nelson's
composition "Bifurcate me, Baby!"

The basic idea is to evaluate the logistic growth function at
different points and convert the value to a musical note.  The growth
function is given by

  x_(n+1) = r x_n (1 - x_n)

We start with an initial population x_0 and iteratively apply the
growth function to it, where r is the growth rate.  For certain values
of r, the population stablizes to a certain value, but as r increases,
the period doubles, quadruples, and eventually leads to chaos.  It is
one of the classic examples in chaos theory.

First we define the growth function which, given a rate r and
current population x, generates the next population.

> grow :: Double -> Double -> Double
> grow r x = r * x * (1-x)

Then we define a signal 'tick' that pulsates at a given frequency
specified by slider f.  This is the signal that will drive the
simulation.  The timer function takes in a frequency.

The next thing we need is a time-varying population.  This is where 
the delay function and the rec keyword come in handy.  We initialize 
the 'pop' signal with the value 0.1, and then on every tick, we 
grow it with the instantaneous value of the growth rate signal.

We can now write a simple function that maps a population value to a
musical note:

> popToNote :: Double -> [MidiMessage]
> popToNote x = [ANote 0 n 64 0.05] where n = truncate (x * 127)

Finally, to play the note, we simply send the current population to 
popToNote, and send the result to the selected Midi output device.  

> bifurcate = runUIEx (300,500) "Bifurcate!" $ proc _ -> do
>   mo <- selectOutput -< ()
>   f  <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
>   r  <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
>   
>   tick <- timer -< 1.0 / f
>   rec pop <- delay 0.1 -<  maybe pop (const $ grow r pop) tick
>       
>   _ <- title "Population" $ display -< pop
>   midiOut -< (mo, fmap (const (popToNote pop)) tick)


============
Echo example

Here we present a program that takes in a Midi event stream and, in
addition to playing each note received from the input device, it also
echoes the note at a given rate, while playing each successive note
more softly until the velocity reduces to 0.

The key component we need for this problem is a delay function that
can delay a given event signal for a certain amount of time.  vdelay
takes in the amount of time to delay and an input signal
and outputs the delayed signal.

There are two signals we want to attenuate.  One is the signal coming
from the input device, and the other is the delayed and decayed signal
containing the echoes.  In the code shown below, they are denoted as m
and s, respectively.  We merge the two event streams into one and then 
remove events with empty Midi messages by replacing them with Nothing.  
The resulting signal, m', is then sent to the Midi output device.

The echo signal s is created recursively from m' as follows.  We examine 
the signal m' and decay any events that we find there, using the decay 
rate indicated by the instantaneous value from the slider r.  This 
decayed signal is fed into the vdelay signal function along with 
the amount of time to delay (the inverse of the echo frequency, 
which is given by the other slider f).

> echo = runUIEx (500,500) "Echo" $ proc _ -> do
>   mi <- selectInput  -< ()
>   mo <- selectOutput -< ()
>   m <- midiIn -< mi
>   r <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.5) -< ()
>   f <- title "Echoing frequency" $ withDisplay (hSlider (1, 10) 10) -< ()
>   
>   rec let m' = removeNull $ mergeS m s
>       s <- vdelay -< (1.0 / f, fmap (mapMaybe (decay 0.1 r)) m')
>   
>   midiOut -< (mo, m')

> mergeS :: Maybe [MidiMessage] -> Maybe [MidiMessage] -> Maybe [MidiMessage]
> mergeS (Just ns1) (Just ns2) = Just (ns1 ++ ns2)
> mergeS n1         Nothing    = n1
> mergeS Nothing    n2         = n2

> removeNull :: Maybe [MidiMessage] -> Maybe [MidiMessage]
> removeNull Nothing   = Nothing
> removeNull (Just []) = Nothing
> removeNull (Just xs) = Just xs

> decay :: Time -> Double -> MidiMessage -> Maybe MidiMessage
> decay dur r m = 
>   let f c k v d = if v > 0 
>                   then Just (ANote c k (truncate (fromIntegral v * r)) d)
>                   else Nothing
>   in case m of
>     ANote c k v d -> f c k v d
>     Std (NoteOn c k v) -> f c k v dur
>     _ -> Nothing

