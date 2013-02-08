> {-# LANGUAGE Arrows #-}

> module IntervalTrainer where

> import Prelude hiding (init)
> import Control.Arrow
> import Control.CCA.Types

> import Euterpea hiding (Event)
> import Euterpea.IO.MUI
> import Euterpea.IO.MIDI.MidiIO
> import System.IO.Unsafe (unsafePerformIO)
> import System.Random (randomRIO)
> import qualified Codec.Midi as Midi

> import Control.Arrow
> import Control.SF.AuxFunctions 
>   (Event, edgeE, accum, mergeE, 
>   (=>>), (->>), (.|.),
>   snapshot, snapshot_,
>   hold, now)


> main = runUIEx (600,700) "Interval Trainer" intervalTrainer

> -- music theory name for intervals:
> intNameList :: [String]
> intNameList =
>   ["uni","min2","Maj2","min3","Maj3","4th","aug4",
>    "5th","min6","Maj6","min7","Maj7","oct"]

States of the MUI's internal Finite State Machine:

> data State = Start | Base | Guessed
>   deriving (Eq,Ord,Show)

State transition table:

        | Next    | Repeat  | Giveup  | Guess   | Reset   |
-----------------------------------------------------------
Start   | Base    | Start   | Start   | Start   | Start   | 
Base    | Base    | Base    | Guessed | Guessed | Start   |
Guessed | Base    | Guessed | Guessed | Guessed | Start   |

State variables:

total:   number ofintervals generated
correct: number guessed correctly
repeats: number of repeat requests prior to making a guess
answer:  a pair, the random root note and the random interval
state:   the durrect FSA state (see above)

State variable updates:

Variable | Event : action
------------------------------------------------------------------------
total    | Next (Base) : incr, Guess (Base) : incr, Giveup (Base) : incr
correct  | Guess (Base) /\ match : incr
repeats  | Repeat (Base) : incr
answer   | Next : generate and save new random root and interval
state    | see State Transition Table

Also, Reset forces total, correct, and repeats to 0, and answer to (0,0).

The main UI:

> intervalTrainer :: UISF () ()
> intervalTrainer = proc _ -> do
>     -- get running time
>     t <- time -< ()
>     -- MIDI output select:
>     mo <- setSize (600,90) $ selectOutput -< ()
>     -- Play note:
>     pns <- setSize (600,60) . title "Play notes" . leftRight $
>             radio ["Together","Low then high","High then low"] 0 -< ()
>     -- Note length:
>     dur <- setSize (600,60) . title "Note length" . leftRight $ 
>             radio ["Whole","Half","Quarter","Eighth"] 2 -< ()
>     -- Max interval
>     maxInt <- (| (setSize (600,60) . title "Maximum interval" . leftRight) (do
>                 max <- shiSlider 1 (1,12) 12 -< ()
>                 sDisplay -< intNameList !! max
>                 returnA -< max )|)
>     -- Range:
>     range  <- (| (setSize (600,60) . title "Range in octaves" . leftRight) (do
>                 range <- shiSlider 1 (2,10) 4 -< ()
>                 sDisplay -< take 3 $ show $ fromIntegral range / 2
>                 returnA -< range )|)
>     -- Lowest octave:
>     lowOct <- (| (setSize (600,60) . title "Lowest octave" . leftRight) (do
>                 low <- shiSlider 1 (1,8) 4 -< ()
>                 sDisplay -< show low
>                 returnA -< low )|)
>     -- Instrument:
>     instr <- setSize (600,60) . title "Instrument" . leftRight $ 
>               radio ["Acous Piano","Elec Piano","Violin","Saxophone","Flute"] 0 -< ()
>     -- Control:
>     (nextE,repeatE,giveUpE,resetE) <- (| (setSize (600,60) . title "Control" . leftRight) (do
>         next   <- edgeE <<< sButton "Next"      -< ()
>         repeat <- edgeE <<< sButton "Repeat"    -< ()
>         giveUp <- edgeE <<< sButton "Give Up"   -< ()
>         reset  <- edgeE <<< sButton "Reset"     -< ()
>         returnA -< (next,repeat,giveUp,reset) )|)
>     -- User Input:
>     guesses <- (| (setSize (600,90) . title "Guess the interval") (do
>         g1 <- leftRight $
>                 mapA $ map (\s -> edgeE <<< sButton s) 
>                            ["uni","min2","Maj2","min3","Maj3","4th","aug4"] -< repeat ()
>         g2 <- leftRight $
>                 mapA $ map (\s -> edgeE <<< sButton s)
>                            ["5th","min6","Maj6","min7","Maj7","oct"] -< repeat ()
>         returnA -< g1++g2) |)
>     -- edge-detect pushbuttons:
>     let guessesE = foldl1 (.|.) $ zipWith (->>) guesses intNameList
>     rec -- the state
>         state    <- accum Start -< updates
>         -- event filter based on MUI state
>         let whileIn' :: Event a -> State -> Event a
>             e `whileIn'` s = if s == state then e else Nothing
>             updates  = (giveUpE `whileIn'` Base ->> const Guessed)         .|.
>                        (nextE ->> const Base) .|. (resetE ->> const Start) .|.
>                        (guessesE `whileIn'` Base ->> const Guessed)
>     let whileIn :: Event a -> State -> Event a
>         e `whileIn` s = if s == state then e else Nothing
>  
>     -- Random intervals:
>     let randIntE = snapshot_ nextE (maxInt, lowOct, range) =>> mkRandInt
>     interval <- hold (0,0)  -< randIntE
>     let trigger  = snapshot randIntE (dur, instr) .|.
>                    snapshot_ repeatE (interval, (dur, instr))
>     -- state variables:
>     let matchE   = snapshot (guessesE `whileIn` Base) interval =>> 
>                     \(g,(r,i)) -> if g==intNameList!!i then succ else id
>     total   <- accum 0 -< ((guessesE `whileIn` Base ->> succ) .|.
>                            (nextE    `whileIn` Base ->> succ) .|.
>                            (giveUpE  `whileIn` Base ->> succ) .|.
>                            (resetE ->> const 0)                  )
>     correct <- accum 0 -< (matchE .|. (resetE ->> const 0))
>     repeats <- accum 0 -< ((repeatE `whileIn` Base ->> succ) .|.
>                            (resetE ->> const 0)                  )
>     -- Note delays
>     let f n pn dur = if pn==n then 1 / fromIntegral (2 ^ dur) else 0
>         del0 = f 2 pns dur -- lo note delay only when "hi then lo"
>         del1 = f 1 pns dur -- hi note delay only when "lo then hi"
>     -- Random interval & Midi signals:
>     note0 <- delayt -< (t, del0, (trigger =>> mkNote 0))
>     note1 <- delayt -< (t, del1, (trigger =>> mkNote 1))
>     nowE <- now -< ()
>     let progChan = nowE ->> (map Std $
>                     zipWith Midi.ProgramChange [0,1,2,3,4] [0,4,40,66,73])
>         midiMsgs = progChan .|. mergeE (++) note0 note1
>     -- Display results:
>     (| leftRight (do
>         title "Score:"   $ setSize (120,50) $ 
>                             display -< showScore correct total
>         title "Repeats:" $ setSize (120,50) $ display -< show repeats
>         title "Answer:"  $ setSize (120,50) $ display -< 
>                 if state==Guessed then intNameList!!(snd interval) else ""
>         returnA -< () )|)
>     -- Midi output
>     midiOut -< (mo, midiMsgs)
>     returnA -< ()


Auxilliary Functions:

> sDisplay              = setSize (50,25) display
> shiSlider inc ran pre = setSize (300,25) $ hiSlider inc ran pre
> sButton str           = setSize (75,25)  $ buttonE str

> buttonE :: String -> UISF () (Event ())
> buttonE s = button s >>> arr (\b -> if b then Just () else Nothing)

> showScore     :: Int -> Int -> String
> showScore c 0 = "0"
> showScore c t = show c ++ "/" ++ show t ++ " = " ++ 
>                 take 5 (show (100 * fromIntegral c / fromIntegral t)) ++ "%"

> mkRandInt :: (Int,Int,Int) -> (Int,Int)
> mkRandInt (maxInt,lowOct,range) = 
>   unsafePerformIO $ do
>     let low = lowOct*12
>     int  <- randomRIO (0,maxInt) :: IO Int
>     root <- randomRIO (low, low + range*6 - int) :: IO Int
>     return (root,int)

> mkNote :: Int -> ((Int,Int),(Int,Int)) -> [MidiMessage]
> mkNote n ((root,int),(dur,instr)) =
>   let durT = 1 / fromIntegral (2 ^ dur)
>   in if n==0 then [ANote instr root 100 durT]
>              else [ANote instr (root+int) 100 durT]

0 whole   1   sec  1/2^0
1 half    1/2 sec  1/2^1
2 quarter 1/4 sec  1/2^2
3 eighth  1/8 sec  1/2^3

at 60 BPM a whole note is 1 sec

ANote :: Channel -> Key -> Velocity -> Time -> MidiMessage


> mapA :: Arrow a => [a b c] -> a [b] [c]
> mapA [] = arr $ const []
> mapA (sf:sfs) = proc (b:bs) -> do
>     c <- sf -< b
>     cs <- mapA sfs -< bs
>     returnA -< (c:cs)




