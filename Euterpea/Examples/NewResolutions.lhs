New Resolutions by Jean-Luc Ponty, Scott O'Neil, and John Garvin

> module Euterpea.Examples.NewResolutions where
> import Euterpea

> nrContext = Context {cTime = 0,
>                      cPlayer = fancyPlayer,
>                      cInst = Marimba,
>                      cDur = 1.0,
>                      cKey = 0,
>                      cVol = 100}
>
> tNewRes m = makeMidi (m, nrContext, defUpm)

> root, minThird, fifth, octave :: Pitch -> Dur -> Music Pitch
> root       p dur = Prim $ Note dur p
> minThird   p dur = Prim $ Note dur (trans 3 p)
> majThird   p dur = Prim $ Note dur (trans 4 p)
> fifth      p dur = Prim $ Note dur (trans 7 p)
> majSixth   p dur = Prim $ Note dur (trans 9 p)
> minSeventh p dur = Prim $ Note dur (trans 10 p)
> octave     p dur = Prim $ Note dur (trans 12 p)
> oMinThird  p dur = Prim $ Note dur (trans 15 p)
> oFifth     p dur = Prim $ Note dur (trans 19 p)

> minArpegUp, minArpegDown :: Pitch -> Dur -> Music Pitch
> minArpegUp p d = root p d
>                  :+: minThird p d
>                  :+: fifth p d 
>                  :+: octave p d
> minArpegDown p d = octave p d
>                :+: fifth p d
>                :+: minThird p d
>                :+: root p d
> majArpegDown p d = octave p d
>                :+: fifth p d
>                :+: majThird p d
>                :+: root p d
> six3ArpegDown p d = octave p d
>                 :+: majSixth p d
>                 :+: majThird p d
>                 :+: root p d

> pattern = minArpegUp (D,5) sn
>       :+: minArpegDown (C,5) sn
>       :+: minArpegUp (A,4) sn
>       :+: minArpegDown (G,4) sn
>       :+: minArpegUp (F,4) sn
>       :+: d 5 sn :+: a 4 sn :+: f 4 sn :+: a 4 sn

> melPattern = d 6 en :+: c 6 en :+: d 6 en
>          :+: snr
>          :+: a 5 en :+: g 5 en :+: a 5 en

> melody1 = melPattern :+: enr :+: d 5 sn
>       :+: f 5 sn :+: g 5 en :+: f 5 sn :+: d 5 en :+: c 5 en
>       :+: d 5 en :+: melPattern :+: d 5 sn
>       :+: f 5 sn :+: f 5 sn :+: g 5 sn :+: f 5 sn
>       :+: d 5 sn :+: c 5 en :+: d 5 den
>       :+: melPattern :+: d 5 sn
>       :+: f 5 sn :+: g 5 sn :+: f 5 sn :+: d 5 en
>       :+: c 5 sn :+: d 5 en
>       :+: d 6 en :+: c 6 en :+: d 6 den :+: c 6 en
>       :+: a 5 en :+: c 6 en :+: a 5 sn :+: g 5 en
>       :+: f 5 en :+: af 5 en
>       :+: g 5 sn :+: f 5 sn :+: d 5 sn :+: c 5 sn 
> -- last note removed to make fit with pattern

> bellPart = d 7 en :+: f 7 en :+: c 7 en :+: d 7 en
>        :+: a 6 en :+: c 7 en :+: g 6 en :+: a 6 en
>        :+: f 6 en :+: g 6 en
>        :+: d 6 sn :+: f 6 sn :+: a 6 sn :+: c 7 sn

> vibesLine = d 5 qn :+: c 5 qn :+: a 4 qn
>         :+: g 4 qn :+: f 4 qn :+: d 4 qn
> vibesPart = vibesLine :=: Modify (Transpose 12) vibesLine

> cMajorScale = [(C,0), (D,0), (E,0), (F,0), (G,0), (A,0), (B,0)]
> gMajorScale = [(G,0), (A,0), (B,0), (C,1), (D,1), (E,1), (Fs,1)]
> dPentMinScale = [(D,0), (F,0), (G,0), (A,0), (C,1)]

> prevNote []         _       = error ("Scale empty")
> prevNote [x]        _       = error ("Note not found in scale")
> prevNote ((y,n):ys) (p,oct) | y == p = let (x,m) = last ys
>                                        in (x, oct + m - n - 1)
> prevNote ((x,m):(y,n):xys) (p,oct) | y == p    = (x, oct + m - n)
>                                    | otherwise = prevNote ((y,n):xys) (p,oct)

> nextNote scale note = nextNote' (head scale) scale note
> nextNote' _ [] _ = error ("Scale empty")
> nextNote' (fstP,fstO) [(x,m)]           (p,oct)
>                                       | x == p    = (fstP, oct - m + fstO + 1)
>                                       | otherwise = error ("Note not found in scale")
> nextNote' fst         ((x,m):(y,n):xys) (p,oct)
>                                       | x == p    = (y, oct - m + n)
>                                       | otherwise = nextNote' fst ((y,n):xys) (p,oct)

> back2Note s = prevNote s . prevNote s

> nextNR = nextNote dPentMinScale
> prevNR = prevNote dPentMinScale
> back2NR = back2Note dPentMinScale

> diddle p = snr :+: Prim (Note sn p) 
>            :+: Prim (Note sn (prevNR p)) :+: Prim (Note sn p)

> melody2 = d 6 sn :+: d 6 en :+: c 6 en :+: d 6 sn :+: c 6 en
>       :+: a 5 en :+: g 5 sn :+: f 5 sn
>       :+: g 5 sn :+: f 5 sn :+: d 5 sn :+: f 5 sn
>       :+: diddle (D,5) :+: diddle (C,5)
>       :+: diddle (D,6) :+: diddle (C,6) :+: diddle (A,5)
>       :+: diddle (G,5) :+: diddle (F,5) :+: diddle (D,5)
>       :+: snr :+: d 6 en :+: c 6 en :+: d 6 den
>       :+: c 6 en :+: a 5 en :+: g 5 den
>       :+: f 5 en :+: g 5 en :+: f 5 sn
>       :+: g 5 sn :+: f 5 sn :+: d 5 sn :+: c 5 sn
>       :+: d 5 den :+: d 6 en :+: c 6 den :+: a 5 en :+: g 5 den
>       :+: f 5 en :+: d 5 den :+: c 5 en :+: d 5 qn

> part1 = Modify (Instrument Marimba) (Modify (Phrase [Dyn (Loudness 70)]) pattern)
>         :+:
>         Modify (Instrument Xylophone) (Modify (Phrase [Dyn (Loudness 120)]) melody1)
>     :=: Modify (Instrument Marimba) (Modify (Phrase [Dyn (Loudness 70)]) (timesM 4 pattern))
> bridge = Modify (Instrument Xylophone) (d 5 hn) -- (d 5 hn [Volume 120])
>      :=: (timesM 2 $
>          Modify (Instrument Marimba) (Modify (Phrase [Dyn (Loudness 60)]) (Modify (Transpose (-12)) bellPart))
>      :=: Modify (Instrument Vibraphone) (Modify (Phrase [Dyn (Loudness 40)]) vibesPart)
>      :=: Modify (Instrument Glockenspiel) (Modify (Phrase [Dyn (Loudness 80)]) bellPart))
> part2 = Modify (Instrument Xylophone) (Modify (Phrase [Dyn (Loudness 120)]) melody2)
>     :=: Modify (Instrument Marimba) (Modify (Phrase [Dyn (Loudness 70)]) (timesM 3 pattern
>                                                  :+: minArpegUp   (D,5) sn
>                                                  :+: minArpegDown (C,5) sn
>                                                  :+: minArpegUp   (A,4) sn
>                                                  :+: minArpegDown (G,4) sn
>                                                  :+: minArpegUp   (F,4) sn
>                                                  :+: d 5 sn))
>     :=: timesM 4 (Modify (Instrument Vibraphone) (Modify (Phrase [Dyn (Loudness 40)]) vibesPart))

> run1 p d = root p d       :+: minThird p d  :+: fifth p d
>        :+: minSeventh p d :+: octave p d    :+: oMinThird p d
>        :+: oFifth p d     :+: oMinThird p d :+: octave p d
>        :+: minSeventh p d :+: fifth p d      :+: minThird p d

> part3Pattern el = el (D,4) sn :+: el (C,4) sn :+: el (D,4) sn :+: el (F,4) sn

> run2 p d = timesM 2 $
>       fifth p d     :+: minSeventh p d :+: octave p d
>   :+: oMinThird p d :+: octave p d     :+: minSeventh p d

> run3 p d = timesM 3 $
>       oMinThird p d :+: octave p d :+: minSeventh p d :+: fifth p d

> vibeLine3 = let el = \p -> octave p den :+: fifth p den
>                        :+: minSeventh p den :+: octave p den
>             in el (D,4) :+: el (C,4) :+: el (D,4)
>                :+: f 5 den :+: c 5 den
>                :+: ef 5 en :+: f 5 en :+: af 5 en
> vibePart3 = vibeLine3 :=: Modify (Transpose 12) vibeLine3

> melody3 = a 5 (11/16) :+: f 6 sn
>       :+: ef 6 en :+: d 6 en :+: c 6 en :+: g 5 dqn
>       :+: timesM 3 (a 5 sn :+: f 6 en) :+: a 5 en
>       :+: f 6 en :+: af 5 en :+: f 6 en :+: af 5 en
>       :+: minArpegDown (F,5) sn :+: snr
>       :+: majArpegDown (F,5) sn :+: snr
>       :+: six3ArpegDown (F,5) sn :+: snr :+: f 6 sn :+: d 6 sn
>       :+: ef 6 sn :+: d 6 sn :+: c 6 sn :+: g 5 sn :+: snr
>       :+: majArpegDown (Ef,5) sn :+: snr :+: ef 6 sn :+: c 6 sn
>       :+: majArpegDown (F,5) sn :+: snr
>       :+: six3ArpegDown (F,5) sn :+: snr :+: f 6 sn :+: d 6 sn
>       :+: minArpegDown (F,5) sn :+: snr
>       :+: minArpegDown (F,5) sn :+: af 5 sn :+: c 6 sn :+: f 6 sn
>       :+: line (map (timesM 2) [f 6 sn, d 6 sn, c 6 sn,
>                                a 5 sn, g 5 sn, f 5 sn])
>       :+: ef 5 sn :+: f 5 sn :+: g 5 sn :+: bf 5 sn
>       :+: c 6 sn :+: d 6 sn :+: ef 6 sn :+: d 6 sn
>       :+: c 6 sn :+: bf 5 sn :+: a 5 sn :+: g 5 sn
>       :+: timesM 4 (a 5 sn :+: a 5 sn :+: g 5 sn)
>       :+: timesM 2 (af 5 sn :+: af 5 sn :+: g 5 sn)
>       :+: timesM 2 (af 5 sn :+: g 5 sn :+: f 5 sn)
>       :+: a 5 dqn
>       :+: f 6 sn :+: d 6 sn :+: c 6 sn
>       :+: a 5 sn :+: g 5 sn :+: f 5 sn
>       :+: g 5 sn :+: bf 5 sn :+: ef 6 dqn
>       :+: bf 6 den :+: bf 6 sn
>       :+: a 6 en :+: a 6 sn :+: g 6 en :+: g 6 sn
>       :+: f 6 den :+: a 5 sn :+: c 6 sn :+: d 6 sn
>       :+: f 6 den :+: f 6 sn :+: d 6 sn :+: c 6 sn
>       :+: af 5 sn :+: af 5 sn :+: g 5 sn
>       :+: f 5 sn :+: d 5 sn :+: c 5 sn

> harmony3 = Modify (Phrase [Dyn (Loudness 60)]) (part3Pattern run1
>                                    :=: part3Pattern run2
>                                    :=: Modify (Transpose 12) (part3Pattern run3))
>        :=: Modify (Phrase [Dyn (Loudness 50)]) (Modify (Instrument Vibraphone) vibePart3)

> part3 = Modify (Phrase [Dyn (Loudness 60)]) (part3Pattern run1)
>     :+: (Modify (Phrase [Dyn (Loudness 60)]) (part3Pattern run1)
>          :=: Modify (Phrase [Dyn (Loudness 90)]) (part3Pattern run2))
>     :+: (Modify (Phrase [Dyn (Loudness 60)]) ((part3Pattern run1) 
>                                               :=: (part3Pattern run2))
>          :=: Modify (Phrase [Dyn (Loudness 100)]) (Modify (Transpose 12) (part3Pattern run3)))
>     :+: Modify (Phrase [Dyn (Loudness 60)]) (part3Pattern run1
>                                              :=: part3Pattern run2
>                                              :=: Modify (Transpose 12) (part3Pattern run3))
>     :=: Modify (Phrase [Dyn (Loudness 70)]) (Modify (Instrument Vibraphone) vibePart3)
>     :+: (timesM 4 harmony3 :=: Modify (Phrase [Dyn (Loudness 100)]) (Modify (Instrument Xylophone) melody3)
>                                                        :=: (Modify (Instrument Marimba) melody3))

> all3Insts m = Modify (Instrument Marimba) m
>           :=: Modify (Instrument Xylophone) m
>           :=: Modify (Instrument Vibraphone) m

> endEl n = Prim (Note sn n)          :+: Prim (Note sn (back2NR n))
>       :+: Prim (Note sn (prevNR n)) :+: Prim (Note sn n)

> endRun = line $ map endEl $ take 10 $ iterate nextNR (D,5)

> ending = all3Insts $
>       Prim (Note qn (D,5))
>   :+: Modify (Phrase [Dyn (Loudness 120)]) (endRun :+: d 7 sn)

> newResolutions = part1 :+: bridge :+: part2 :+: part3 :+: ending

> nr = play newResolutions
