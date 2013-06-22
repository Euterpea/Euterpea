module Main where
import Euterpea
import Euterpea.Music.Signal.SigFuns hiding (main)
import System.CPUTime
import Text.Printf
import Control.Arrow

vibSF :: AudSF () Double
vibSF = constA 440 >>> vibrato 5 20

main = do
    t1 <- getCPUTime
    outFile "out.wav" 120.0 vibSF
    t2 <- getCPUTime
    let t = fromIntegral (t2-t1) * 1e-12 :: Double
    printf "Took %6.2fs\n" t
