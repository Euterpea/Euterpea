{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Control.SF.AuxFunctions (
    Event, edge, quantize,
    presentFFT, fftA, 
    toMSF, toRealTimeMSF, 
    Control, buffer
) where

import Prelude hiding (init)
import Control.Arrow
import Control.CCA.Types

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
-- Generic Types and Functions
--------------------------------------

type Event a = Maybe a

edge :: ArrowInit a => a Bool Bool
edge = proc b -> do
    prev <- init False -< b
    returnA -< prev && not b

-- | Scrutinizes n samples at a time, updating after k new values from a signal function
quantize :: ArrowInit a => Int -> Int -> a b (Event [b])
quantize n k = proc d -> do
    rec (ds,c) <- init ([],0) -< (take n (d:ds), c+1)
    returnA -< if c >= n && c `mod` k == 0 then Just ds else Nothing



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
fftA :: ArrowInit a => Int -> Int -> a Double (Event [Double])
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
