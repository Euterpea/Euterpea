> {-# LANGUAGE Arrows #-}
> module Euterpea.Examples.BasicSignalExamples where
>
> import Euterpea
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.Music.Signal.SigFuns (AudSF)

> mySig :: AudSF () Double
> mySig = proc _ -> do
>   e <- countTime 100 milliseconds -< ()
>   



> main = outFile "sample.wav" 5 mySig
