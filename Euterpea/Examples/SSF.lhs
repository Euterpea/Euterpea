The first phrase of the flute part of "Stars and Stripes Forever."

> module Euterpea.Examples.SSF where
> import Euterpea
>
> legato = Legato (11/10)
> staccato = Staccato (5/10)
>
> ssfMelody = line (m1 ++ m2 ++ m3 ++ m4)
> m1 = [                                         trilln 2 5 (bf 6 en),
> 	Modify (Phrase [Art staccato])    (line [ef 7 en,
>                                                ef 6 en,
>                                                ef 7 en])]
>
> m2 = [Modify (Phrase [Art legato])      (line [bf 6 sn,
>                                                c  7 sn,
>                                                bf 6 sn,
>                                                g  6 sn]),
>	Modify (Phrase [Art staccato])    (line [ef 6 en,
>                                                bf 5 en])]
>
> m3 = [Modify (Phrase [Art legato])      (line [ef 6 sn,
>                                                f  6 sn,
>                                                g  6 sn,
>                                                af 6 sn]),
>	Modify (Phrase [Art staccato])    (line [bf 6 en,
>                                                ef 7 en])]
>
> m4 = [                                         trill 2 tn (bf 6 qn),
>                                                bf 6 sn,
>                                                denr]
>
> ssf = Modify (Instrument Flute) ssfMelody
