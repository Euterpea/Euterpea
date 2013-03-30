> module Euterpea.Examples.EuterpeaExamples where
>
> import Euterpea
> import Euterpea.Examples.Interlude
> import Euterpea.Examples.SelfSimilar
> import Euterpea.Examples.SSF

Simple examples of Euterpea in action.  Note that this module also
imports modules Interlude and SelfSimilar.

-----------------------------------------------------------------------------

From the tutorial, try things such as pr12, cMajArp, cMajChd, etc. and
try applying inversions, retrogrades, etc. on the same examples.  Also
try "childSong6" imported from module Interlude.  For example:

> t0 = play childSong6

-----------------------------------------------------------------------------

C Major scale for use in examples below:

> cMajScale = Modify (Tempo 2)
>             (line [c 4 en, d 4 en, e 4 en, f 4 en, 
>                    g 4 en, a 4 en, b 4 en, c 5 en])
>
> cms' = line [c 4 en, d 4 en, e 4 en, f 4 en, 
>              g 4 en, a 4 en, b 4 en, c 5 en]
>
> cms = cMajScale

Test of various articulations and dynamics:

> t1 = play (Modify (Instrument Percussion)
>        (Modify (Phrase [Art (Staccato (1/10))]) cms :+:
>         cms                             :+:
>         Modify (Phrase [Art (Legato  (11/10))]) cms    ))
>
> temp = Modify (Instrument AcousticGrandPiano) 
>          (Modify (Phrase [Dyn (Crescendo 4)]) (c 4 en))
>
> mu2 = Modify (Instrument Vibraphone)
>        (Modify (Phrase [Dyn (Diminuendo (3/4))]) cms :+:
>          (Modify (Phrase [Dyn (Crescendo 4), Dyn (Loudness 25)]) cms))
> t2 = play mu2
>
> t3 = play (Modify (Instrument Flute) 
>        (Modify (Phrase [Tmp (Accelerando 0.3)]) cms :+:
>         Modify (Phrase [Tmp (Ritardando  0.6)]) cms    ))

-----------------------------------------------------------------------------

A function to recursively apply transformations f (to elements in a
sequence) and g (to accumulated phrases):

> {-
> rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> 
>        Music a -> Music a
> rep f g 0 m = Prim (Rest 0)
> rep f g n m = m :=: g (rep f g (n-1) (f m))

An example using "rep" three times, recursively, to create a "cascade"
of sounds.

> run       = rep (Modify (Transpose 5)) (delayM tn) 8 (c 4 tn)
> cascade   = rep (Modify (Transpose 4)) (delayM en) 8 run
> cascades  = rep  id       (delayM sn) 2 cascade
> -}

> t4' x     = play (Modify (Instrument AcousticGrandPiano) x)
> t4        = play (Modify (Instrument AcousticGrandPiano) 
>               (cascades :+: revM cascades))

What happens if we simply reverse the f and g arguments?

> {-
> run'      = rep (delayM tn) (Modify (Transpose 5)) 4 (c 4 tn)
> cascade'  = rep (delayM en) (Modify (Transpose 4)) 6 run'
> cascades' = rep (delayM sn)  id       2 cascade'
> -}

> t5        = play (Modify (Instrument AcousticGrandPiano) cascades')

-----------------------------------------------------------------------------

Example from the SelfSimilar module.

> t10s   = play (rep (delayM (dur ttm0)) (Modify (Transpose 4)) 2 ttm0)

-----------------------------------------------------------------------------

Example from the Interlude module.

> cs6 = play childSong6

-----------------------------------------------------------------------------

Example from the Ssf (Stars and Stripes Forever) module.

> ssf0 = play ssf

-----------------------------------------------------------------------------

Midi percussion test.  Plays all "notes" in a range.  (Requires adding
an instrument for percussion to the UserPatchMap.)

> drums a b = Modify (Instrument Percussion)
>                   (line (map (\p-> Prim $ Note sn (pitch p)) [a..b]))
> t11 a b = play (drums a b)

-----------------------------------------------------------------------------

Test of cut and shorten.

> t12  = play (cut 4 childSong6)
> t12a = play (cms /=: childSong6)

-----------------------------------------------------------------------------

Tests of the trill functions.

> t13note = Prim (Note qn (C,5))
> t13 =  play (trill   1 sn t13note)
> t13a = play (trill'  2 dqn t13note)
> t13b = play (trilln  1 5 t13note)
> t13c = play (trilln' 3 7 t13note)
> t13d = play (roll tn t13note)
> t13e = play (Modify (Tempo (2/3)) 
>               (Modify (Transpose 2) 
>                 (Modify (Instrument AcousticGrandPiano) 
>                   (trilln' 2 7 t13note))))

-----------------------------------------------------------------------------

Tests of drum.

> t14 = play (Modify (Instrument Percussion) (perc AcousticSnare qn))

> -- a "funk groove"
> t14b = let p1 = perc LowTom        qn
>            p2 = perc AcousticSnare en
>        in play (Modify (Tempo 3) (Modify (Instrument Percussion) (cut 8 (repeatM
>                  ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
>                    p1 :+: p1 :+: qnr :+: p2 :+: enr)
>                   :=: roll en (perc ClosedHiHat 2))))))

> -- a "jazz groove"
> t14c = let p1 = perc CrashCymbal2  qn
>            p2 = perc AcousticSnare en
>            p3 = perc LowTom        qn
>        in play (Modify (Tempo 3) (Modify (Instrument Percussion) (cut 4 (repeatM
>                  ((p1 :+: (Modify (Tempo (3/2)) (p2 :+: enr :+: p2))
>                   :=: (p3 :+: qnr)) )))))

> t14d = let p1 = perc LowTom        en
>            p2 = perc AcousticSnare hn
>        in play (Modify (Instrument Percussion)
>                   (  roll tn p1
>                  :+: p1
>                  :+: p1
>                  :+: Prim (Rest en)
>                  :+: roll tn p1
>                  :+: p1
>                  :+: p1
>                  :+: Prim (Rest qn)
>                  :+: roll tn p2
>                  :+: p1
>                  :+: p1  ))

-----------------------------------------------------------------------------

Tests of the MIDI interface.

> loadMidiFile fn = do
>   r <- importFile fn 
>   case r of
>     Left err -> error err
>     Right m  -> return m

Music into a MIDI file.

> tab m = do
>           exportFile "test.mid" $ makeMidi (m, defCon, defUpm)

Music to a MidiFile datatype and back to Music.

> tad m = fromMidi (testMidi m)

A MIDI file to a MidiFile datatype and back to a MIDI file.

> tcb file = do
>              x <- loadMidiFile file
>              exportFile "test.mid" x

MIDI file to MidiFile datatype.

> tc file = do
>             x <- loadMidiFile file
>             print x

MIDI file to Music, a UserPatchMap, and a Context.

> tcd file = do
>              x <- loadMidiFile file
>              print $ fst3 $ fromMidi x
>              print $ snd3 $ fromMidi x
>              print $ thd3 $ fromMidi x

A MIDI file to Music and back to a MIDI file.

> tcdab file = do
>              x <- loadMidiFile file
>              exportFile "test.mid" $ makeMidi $ fromMidi x

> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd3 (a,b,c) = c

