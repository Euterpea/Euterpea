> module JitterTest where
> import Euterpea
> import PlayS
> import Numeric
> import Data.List

Jitter Analysis
Donya Quick
Last modified: 27-Oct-2014

Expected file format is a series of lines that look like this:

xxxxxxxx (info for a note on)
xxxxxxxx (info for corresponding note off)
etc.

testValue: a series of quarternotes used for checking jitter.

analyzeFile1: checks for distances between events. A playback method that hangs briefly but 
then proceeds perfectly with correcting timing from the first event following the hang will 
get a low jitter score with this approach. Values returned are the minimum, maximum, and 
average jitter using this method of comparison.

analyzeFile2: checks for exact timing of events normalized to the time of the first event
in the series. In other words, the only "free" delay is before the first event is piped 
out. After that, any delays will count towards jitter. Values returned are the minimum, 
maximum, and average jitter using this method of comparison.

showDiffs: shows timestamps in relative form (time since last event) for a particular 
file.


> tpb = 500 -- ticks per quarternote

> testValue = line $ take 100 $ repeat (c 4 qn)
> expectedDiffs = concat $ repeat [tpb,0] -- [noteOff-noteOn, noteOn-noteOff, ...]
> expectedTs = [0,tpb] ++ map (+tpb) expectedTs

> lineTimeTicks :: String -> Int
> lineTimeTicks str = f $ readHex $ takeWhile (not.(`elem` " \t\n")) str where
>     f [] = error ("Could not parse line: "++str)
>     f (x:xs) = fst x

> --          filename     avg     min     max
> analyzeFile1 :: FilePath -> IO (Double, Double, Double)
> analyzeFile1 fpath = do
>     fdata <- readFile fpath
>     let allTicks = map lineTimeTicks $ lines fdata
>     return $ jitter1 allTicks

> --          filename     avg     min     max
> analyzeFile2 :: FilePath -> IO (Double, Double, Double)
> analyzeFile2 fpath = do
>     fdata <- readFile fpath
>     let allTicks = map lineTimeTicks $ lines fdata
>     return $ jitter2 allTicks

> showDiffs :: FilePath -> IO [Int]
> showDiffs fpath = do
>     fdata <- readFile fpath
>     let allTicks = map lineTimeTicks $ lines fdata
>     return $ f allTicks where
>     f ts = zipWith subtract ts $ tail ts

> jitter1 :: [Int] -> (Double, Double, Double)
> jitter1 tStamps = 
>     let tDiffs = zipWith subtract tStamps $ tail tStamps
>         jitters = map (fromIntegral.abs) $ zipWith subtract tDiffs expectedDiffs
>     in  (average jitters, minimum jitters, maximum jitters) 

> jitter2 :: [Int] -> (Double,Double,Double)
> jitter2 tStamps = 
>     let t0 = map (subtract $ head tStamps) tStamps
>         jitters = map (fromIntegral.abs) $ zipWith subtract t0 expectedTs
>     in  (average jitters, minimum jitters, maximum jitters) 


> average xs = sum xs / fromIntegral (length xs)