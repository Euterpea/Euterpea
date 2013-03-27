{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Control.SF.AuxFunctions (
    SEvent, Time, 
    edge, 
    accum, unique, 
    hold, now, 
    mergeE, 
    delay, vdelay, fdelay, 
    timer, genEvents, 
    
    (=>>), (->>), (.|.),
    snapshot, snapshot_,

    toMSF, toRealTimeMSF, 
    quantize, presentFFT, fftA, 
    Control, buffer,
) where

import Prelude hiding (init)
import Control.Arrow
import Control.CCA.Types

import Codec.Midi (Time) -- for reexporting

import Numeric.FFT (fft)
import Data.Complex
import Data.Map (Map)
import qualified Data.Map as Map

-- | Alternative for working with Math.FFT instead of Numeric.FFT
--import qualified Math.FFT as FFT
--import Data.Array.IArray
--import Data.Array.CArray
--myFFT n lst = elems $ (FFT.dft) (listArray (0, n-1) lst)

-- For use with SF Conversions
import Control.Monad.Fix
import Control.SF.SF
import Control.SF.MSF

import Control.Concurrent.MonadIO
import Data.IORef.MonadIO
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Control.DeepSeq


--------------------------------------
-- Types
--------------------------------------

-- | SEvent is short for "Stream Event" and is a type synonym for Maybe
type SEvent = Maybe

-- Time is reexported from Codec.Midi
-- type Time = Double 


--------------------------------------
-- Useful SF Utilities (Mediators)
--------------------------------------

-- | edge generates an event whenever the Boolean input signal changes
--   from False to True -- in signal processing this is called an ``edge
--   detector,'' and thus the name chosen here.
edge :: ArrowInit a => a Bool (SEvent ())
edge = proc b -> do
    prev <- init False -< b
    returnA -< if not prev && b then Just () else Nothing

-- | The signal function (accum v) starts with the value v, but then 
--   applies the function attached to the first event to that value 
--   to get the next value, and so on.
accum :: ArrowInit a => b -> a (SEvent (b -> b)) b
accum x = proc f -> do
    rec b <- init x -< maybe b ($b) f
    returnA -< b

unique :: Eq e => ArrowInit a => a e (SEvent e)
unique = proc e -> do
    prev <- init Nothing -< Just e
    returnA -< if prev == Just e then Nothing else Just e

-- | hold is a signal function whose output starts as the value of the 
--   static argument.  This value is held until the first input event 
--   happens, at which point it changes to the value attached to that 
--   event, which it then holds until the next event, and so on.
hold :: ArrowInit a => b -> a (SEvent b) b
hold x = arr (fmap (const $)) >>> accum x

-- | Now is a signal function that produces one event and then forever 
--   after produces nothing.  It is essentially an impulse function.
now :: ArrowInit a => a () (SEvent ())
now = arr (const Nothing) >>> init (Just ())

-- | mergeE merges two events with the given resolution function.
mergeE :: (a -> a -> a) -> SEvent a -> SEvent a -> SEvent a
mergeE _       Nothing     Nothing     = Nothing
mergeE _       le@(Just _) Nothing     = le
mergeE _       Nothing     re@(Just _) = re
mergeE resolve (Just l)    (Just r)    = Just (resolve l r)

-- | Returns n samples of type b from the input stream at a time, 
--   updating after k samples.  This function is good for chunking 
--   data and is a critical component to fftA
quantize :: ArrowInit a => Int -> Int -> a b (SEvent [b])
quantize n k = proc d -> do
    rec (ds,c) <- init ([],0) -< (take n (d:ds), c+1)
    returnA -< if c >= n && c `mod` k == 0 then Just ds else Nothing


--------------------------------------
-- Delays and Timers
--------------------------------------

-- | delay is a unit delay.  It is the same as init from ArrowInit, but 
--   we rename it to avoid the namespace conflict it has with init from 
--   the standard prelude.
delay :: ArrowInit a => b -> a b b
delay = init

-- NOTE: The following two functions may be better off with implementations
--  that use Data.Sequence

-- | fdelay is a delay function that delays for a fixed amount of time, 
--   given as the static argument.  It returns a signal function that 
--   takes the current time and an event stream and delays the event 
--   stream by the delay amount.
fdelay :: ArrowInit a => Double -> a (Time, SEvent b) (SEvent b)
fdelay d = proc (t, e) -> do
    rec q <- init [] -< maybe q' (\e' -> q' ++ [(t+d,e')]) e
        let (ret, q') = case q of
                [] -> (Nothing, q)
                (t0,e0):qs -> if t >= t0 then (Just e0, qs) else (Nothing, q)
    returnA -< ret

-- | vdelay is a delay function that delays for a variable amount of time.
--   It takes the current time, an amount of time to delay, and an event 
--   stream and delays the event stream by the delay amount.
vdelay :: ArrowInit a => a (Time, Double, SEvent b) (SEvent b)
vdelay = proc (t, d, e) -> do
    rec q <- init [] -< maybe q' (\e' -> q' ++ [(t,e')]) e
        let (ret, q') = case q of
                [] -> (Nothing, q)
                (t0,e0):qs -> if t-t0 >= d then (Just e0, qs) else (Nothing, q)
    returnA -< ret


-- | timer is a variable duration timer.
--   This timer takes the current time as well as the (variable) time between 
--   events and returns an SEvent steam.  When the second argument is non-positive, 
--   the output will be a steady stream of events.  As long as the clock speed is 
--   fast enough compared to the timer frequency, this should give accurate and 
--   predictable output and stay synchronized with any other timer and with 
--   time itself.
timer :: ArrowInit a => a (Time, Double) (SEvent ())
timer = proc (now, i) -> do
    rec last <- init 0 -< t'
        let ret = now >= last + i
            t'  = latestEventTime last i now
    returnA -< if ret then Just () else Nothing
  where
    latestEventTime last i now | i <= 0 = now
    latestEventTime last i now = 
        if now > last + i
        then latestEventTime (last+i) i now
        else last


-- | genEvents is a timer that instead of returning unit events 
--   returns the next element of the input list.  The input list is 
--   assumed to be infinite in length.
genEvents :: ArrowInit a => [b] -> a (Time, Double) (SEvent b)
genEvents lst = proc inp -> do
    e <- timer -< inp
    rec l <- init lst -< maybe l (const $ tail l) e
    returnA -< fmap (const $ head l) e


--------------------------------------
-- Yampa-style utilities
--------------------------------------

(=>>) :: SEvent a -> (a -> b) -> SEvent b
(=>>) = flip fmap
(->>) :: SEvent a -> b -> SEvent b
(->>) = flip $ fmap . const
(.|.) :: SEvent a -> SEvent a -> SEvent a
(.|.) = flip $ flip maybe Just

snapshot :: SEvent a -> b -> SEvent (a,b)
snapshot = flip $ fmap . flip (,)
snapshot_ :: SEvent a -> b -> SEvent b
snapshot_ = flip $ fmap . const -- same as ->>



--------------------------------------
-- Fast Fourier Transform
--------------------------------------

-- | Converts the vector result of a dft into a map from frequency to magnitude.
--   One common use is:
--      fftA >>> arr (fmap $ presentFFT clockRate)
presentFFT :: Double -> [Double] -> Map Double Double
presentFFT clockRate a = Map.fromList $ map mkAssoc (zip [0..(length a)] a) where 
    mkAssoc (i,c) = (freq, c) where
        samplesPerPeriod = fromIntegral (length a)
        freq = fromIntegral i * (clockRate / samplesPerPeriod)

-- | Given a quantization frequency (the number of samples between each 
--   successive FFT calculation) and a fundamental period, this will decompose
--   the input signal into its constituent frequencies.
--   NOTE: The fundamental period must be a power of two!
fftA :: ArrowInit a => Int -> Int -> a Double (SEvent [Double])
fftA qf fp = proc d -> do
    carray <- quantize fp qf -< d :+ 0
    returnA -< fmap (map magnitude . take (fp `div` 2) . fft) carray



--------------------------------------
-- Signal Function Conversions
--------------------------------------

-- | The following two functions are for lifting SFs to MSFs.  The first 
--   one is a quick and dirty solution, and the second one appropriately 
--   converts a simulated time SF into a real time one.
toMSF :: Monad m => SF a b -> MSF m a b
toMSF (SF sf) = MSF h
    where 
      h a = return (b, toMSF sf')
        where (b, sf') = sf a

-- | The clockrate is the simulated rate of the input signal function.
--   The buffer is the amount of time the given signal function is 
--   allowed to get ahead of real time.  The threadHandler is where the 
--   ThreadId of the forked thread is sent.
--
--   The output signal function takes and returns values in real time.  
--   The input must be paired with time, and the return values are the 
--   list of bs generated in the given time step, each time stamped.  
--   Note that the returned list may be long if the clockrate is much 
--   faster than real time and potentially empty if it's slower.
--   Note also that the caller can check the time stamp on the element 
--   at the end of the list to see if the inner, "simulated" signal 
--   function is performing as fast as it should.
toRealTimeMSF :: forall m a b . (Monad m, MonadIO m, MonadFix m, NFData b) => 
                 Double -> Double -> (ThreadId -> m ()) -> SF a b 
              -> MSF m (a, Double) [(b, Double)]
toRealTimeMSF clockrate buffer threadHandler sf = MSF initFun
  where
    -- initFun creates some refs and threads and is never used again.
    -- All future processing is done in sfFun and the spawned worker thread.
    initFun :: (a, Double) -> m ([(b, Double)], MSF m (a, Double) [(b, Double)])
    initFun (a, t) = do
        inp <- newIORef a
        out <- newIORef S.empty
        timevar <- newEmptyMVar
        tid <- liftIO $ forkIO $ worker inp out timevar 1 1 sf
        threadHandler tid
        sfFun inp out timevar (a, t)
    -- sfFun communicates with the worker thread, sending it the input values 
    -- and collecting from it the output values.
    sfFun :: IORef a -> IORef (S.Seq (b, Double)) -> MVar Double 
          -> (a, Double) -> m ([(b, Double)], MSF m (a, Double) [(b, Double)])
    sfFun inp out timevar (a, t) = do
        writeIORef inp a        -- send the worker the new input
        tryPutMVar timevar t    -- update the time for the worker
        b <- atomicModifyIORef out $ S.spanl (\(_,t0) -> t >= t0) --collect ready results
        return (toList b, MSF (sfFun inp out timevar))
    -- worker processes the inner, "simulated" signal function.
    worker :: IORef a -> IORef (S.Seq (b, Double)) -> MVar Double 
           -> Double -> Integer -> SF a b -> IO ()
    worker inp out timevar t count (SF sf) = do
        a <- readIORef inp      -- get the latest input
        let (b, sf') = sf a     -- do the calculation
        s <- deepseq b $ atomicModifyIORef out (\s -> (s S.|> (b, fromIntegral count/clockrate), s))
        t' <- if S.length s > 0 && snd (seqLastElem s) >= t+buffer then takeMVar timevar else return t
        worker inp out timevar t' (count+1) sf'
    seqLastElem s = S.index s (S.length s - 1)



--------------------------------------
-- A Buffering Arrow (beta)
--------------------------------------

data Control = Play | Pause | Record | Dump | Jump Integer
             | ClearEarlier | ClearLater | ClearAll
-- buffer takes a control signal as well as an input stream of data and 
-- returns values appropriate to the control signal:
-- 
buffer :: ArrowInit a => a (Control, b) [b]
buffer = proc (c,x) -> do
    rec s <- init ([],[]) -< s'
        let (ret, s') = case c of
                Play -> next s
                Pause -> ([], s)
                Record -> ([], cons x s)
                Dump -> (llToList s, s)
                Jump n -> ([], nav n s)
                ClearEarlier -> (fst s, ([], snd s))
                ClearLater -> (snd s, (fst s, []))
                ClearAll -> (llToList s, ([],[]))
    returnA -< ret


-- Helper stuff for buffer
-- =======================
type LensList a = ([a],[a])
cons :: a -> LensList a -> LensList a
cons a (e,l) = (a:e, l)

prev :: LensList a -> ([a], LensList a)
prev l@([],_) = ([], l)
prev (a:e, l) = ([a], (e, a:l))

next :: LensList a -> ([a], LensList a)
next l@(_,[]) = ([], l)
next (e, a:l) = ([a], (a:e, l))

nav :: Integer -> LensList a -> LensList a
nav 0 l = l
nav n l | n > 0 = nav (n-1) (snd $ next l)
nav n l | n < 0 = nav (n+1) (snd $ prev l)

llToList :: LensList a -> [a]
llToList (e, l) = e ++ l
