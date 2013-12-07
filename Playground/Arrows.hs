module Main where
import Euterpea
import Euterpea.Music.Signal.SigFuns hiding (main)
import System.CPUTime
import Text.Printf

s1SF, s2SF, s3SF, s4SF, s5SF :: AudSF () Double
s1SF = s1
s2SF = s2
s3SF = s3
s4SF = s4
s5SF = s5

numSeconds = 120.0

runTest fname test = do
    t1 <- getCPUTime
    outFile (fname ++ ".wav") numSeconds test
    t2 <- getCPUTime
    printf "Calculating %s took %4.2fs\n" fname (fromIntegral (t2-t1) * 1e-12 :: Double)

main = do
    t1 <- getCPUTime
    runTest "s1" s1SF
    runTest "s2" s2SF
    runTest "s3" s3SF
    runTest "s4" s4SF
    runTest "s5" s5SF
    t2 <- getCPUTime
    printf "Total time was:     %4.2fs\n" (fromIntegral (t2-t1) * 1e-12 :: Double)
