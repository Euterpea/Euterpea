module EuterpeaTests where
import Control.Monad
import System.Console.ANSI
import System.IO
import Data.IORef
import Text.Show.Functions
import Test.QuickCheck
import Text.Printf
import EuterpeaInstances
import Euterpea  hiding (Color, Red, Black, White, Green)
import Data.List hiding (transpose)

{-  Currently, perf returns the event lists such that for any given time, the set of events
    set to fire at that exact time appear in an arbitrary order.
    Consequently, for now, musical equivalence will be tested only after sorting the event list.
    
    The main problem at this point is that dur :: Music a -> Dur does not always return the 
    correct duration. This breaks tests revM_SelfInverting and Axiom_11_3_8. Tests
    revM_SelfInverting_weak and Axiom_11_3_8_weak attempt to compensate for this error by
    testing a related case.
-}

-- Musical equivalence for Pitches, taken from a modification of the definition from Chapter 11 of HSoM.

musEquiv :: PMap Note1 -> Context Note1 -> Music Pitch -> Music Pitch -> Bool
musEquiv pmap ctx m1 m2 = (sort e1, d1) == (sort e2, d2)
    where (e1,d1) = perf pmap ctx (toMusic1 m1)
          (e2,d2) = perf pmap ctx (toMusic1 m2)

-- Tests for Euterpea.Music.Note.Music

prop_AbsPitch_Pitch ap = absPitch (pitch ap) == ap
    where types = ap :: AbsPitch

prop_Trans_Composition i j p = trans i (trans j p) == trans (i+j) p
    where types = (i :: Int, j :: Int, p :: Pitch)

-- Tests for Euterpea.Music.Note.MoreMusic

prop_Retro_Composition = forAll musicPitchLines $
    \line -> (retro.retro) line == line

prop_Invert_Composition p c = forAll musicPitchLines $
    \line -> musEquiv p c ((invert.invert) line) line
    where types = (p :: PMap Note1, c :: Context Note1)

prop_RetroInvert_Composition p c = forAll musicPitchLines $
    \line -> musEquiv p c ((retroInvert.invertRetro) line) line
    where types = (p :: PMap Note1, c :: Context Note1)

-- The values get too large after a certain point, and GHC runs out of memory.
prop_Dur_Times_Composition m = forAll smallIntegers $ \n ->
    dur (timesM n m) == toRational n * dur m
    where types = m :: Music Pitch

prop_Dur_Take_Composition m = forAll (liftM toRational (choose (0.0, fromRational (dur m)) :: Gen Double)) $ \d ->
    dur (takeM d m) == d
    where types = m :: Music Pitch

prop_Take_Repeat_Id p c m = musEquiv p c (takeM d (repeatM m)) m
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)
          d = dur m
    
prop_Mmap_Id m = mMap id m == m
    where types = m :: Music Pitch

prop_Mmap_Function_Composition f g m = mMap f (mMap g m) == mMap (f.g) m
    where types = (f :: Pitch -> Pitch, g :: Pitch -> Pitch, m :: Music Pitch)

-- The values get too large after a certain point, and GHC runs out of memory.
prop_TimesM_Seq p c m =
    forAll smallIntegers $ \a ->
    forAll smallIntegers $ \b ->
        musEquiv p c (timesM a m :+: timesM b m) (timesM (a+b) m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Mfold_Identity m = mFold Prim (:+:) (:=:) Modify m == m
    where types = m :: Music Pitch

prop_revM_SelfInverting p c m = musEquiv p c ((revM.revM) m) m
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)
    
prop_revM_SelfInverting_weak p c m = musEquiv p c ((revM.revM.revM) m) (revM m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)
    
prop_revM_DurationPreserving m = dur ((revM.revM) m) == dur m
    where types = m :: Music Pitch

-- Tests for Euterpea.Music.Note.Performance

prop_Perf_Id pmap c m = perf pmap c' m == (perform pmap c' m, dur m * cDur c)
    where types = (pmap :: PMap Note1, c :: Context Note1, m :: Music Note1)
          c' = c { cPlayer = defPlayer }

-- Verification of Axioms in HSoM

prop_Axiom_11_2_1 p c m = 
    forAll tempos $ \d1 ->
    forAll tempos $ \d2 ->
        musEquiv p c (tempo d1 (tempo d2 m)) (tempo (d1*d2) m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Axiom_11_2_2 p c m1 m2 = 
    forAll tempos $ \r ->
        musEquiv p c (tempo r (m1 :+: m2)) (tempo r m1 :+: tempo r m2)
    where types = (p :: PMap Note1, c :: Context Note1, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_2_3 p c m = musEquiv p c (tempo 1 m) m
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Theorem_11_2_1 p c m1 m2 = forAll tempos $
    \r -> musEquiv p c (tempo r m1 :+: m2) (tempo r (m1 :+: tempo (1/r) m2))
    where types = (p :: PMap Note1, c :: Context Note1, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_1a p c m = 
    forAll tempos $ \r1 ->
    forAll tempos $ \r2 ->
        musEquiv p c (tempo r1 (tempo r2 m)) (tempo (r1*r2) m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Axiom_11_3_1b ap1 ap2 m = trans ap1 (trans ap2 m) == trans (ap1+ap2) m
    where types = (ap1 :: Int, ap2 :: Int, m :: Pitch)

prop_Axiom_11_3_2a p c m = 
    forAll tempos $ \r1 ->
    forAll tempos $ \r2 ->
        musEquiv p c ((tempo r1 . tempo r2) m) ((tempo r2 . tempo r1) m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Axiom_11_3_2b p1 p2 m = (trans p1 . trans p2) m == (trans p2 . trans p1) m
    where types = (p1 :: Int, p2 :: Int, m :: Pitch)

{- tempo and trans are incompatible. I assume you meant transpose? -}
prop_Axiom_11_3_2c p c t m = 
    forAll tempos $ \r1 ->
        musEquiv p c ((tempo r1 . transpose t) m) ((transpose t . tempo r1) m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Axiom_11_3_3a p c m1 m2 = 
    forAll tempos $ \r ->
        musEquiv p c (tempo r (m1 :+: m2)) (tempo r m1 :+: tempo r m2)
    where types = (p :: PMap Note1, c :: Context Note1, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_3b p c m1 m2 = 
    forAll tempos $ \r ->
        musEquiv p c (tempo r (m1 :=: m2)) (tempo r m1 :=: tempo r m2)
    where types = (p :: PMap Note1, c :: Context Note1, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_3c p c r m1 m2 = musEquiv p c (transpose r (m1 :+: m2)) (transpose r m1 :+: transpose r m2)
    where types = (p :: PMap Note1, c :: Context Note1, r :: AbsPitch, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_3d p c r m1 m2 = musEquiv p c (transpose r (m1 :=: m2)) (transpose r m1 :=: transpose r m2)
    where types = (p :: PMap Note1, c :: Context Note1, r :: AbsPitch, m1 :: Music Pitch, m2 :: Music Pitch)


prop_Axiom_11_3_4a p c m0 m1 m2 = musEquiv p c (m0 :+: (m1 :+: m2)) ((m0 :+: m1) :+: m2)
    where types = (p :: PMap Note1, c :: Context Note1, m0 :: Music Pitch, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_4b p c m0 m1 m2 = musEquiv p c (m0 :=: (m1 :=: m2)) ((m0 :=: m1) :=: m2)
    where types = (p :: PMap Note1, c :: Context Note1, m0 :: Music Pitch, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_5 p c m0 m1 = musEquiv p c (m0 :=: m1) (m1 :=: m0)
    where types = (p :: PMap Note1, c :: Context Note1, m0 :: Music Pitch, m1 :: Music Pitch)

prop_Axiom_11_3_6a p c =
    forAll tempos $ \r ->
        musEquiv p c (tempo r (rest 0)) (rest 0)
    where types = (p :: PMap Note1, c :: Context Note1)

prop_Axiom_11_3_6b p c t = musEquiv p c (transpose t (rest 0)) (rest 0)
    where types = (p :: PMap Note1, c :: Context Note1, t :: AbsPitch)

prop_Axiom_11_3_6c p c m = musEquiv p c (m :+: rest 0) m && musEquiv p c m (rest 0 :+: m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

prop_Axiom_11_3_6d p c m = musEquiv p c (m :=: rest 0) m && musEquiv p c m (rest 0 :=: m)
    where types = (p :: PMap Note1, c :: Context Note1, m :: Music Pitch)

{- I couldn't make sense of Axiom 11.3.7 -}

prop_Axiom_11_3_8 p c m0 m1 m2 m3 =
    musEquiv p c ((m0' :+: m1) :=: (m2' :+: m3)) ((m0' :=: m2') :+: (m1 :=: m3))
    where types = (p :: PMap Note1, c :: Context Note1, m0 :: Music Pitch, 
                   m1 :: Music Pitch, m2 :: Music Pitch, m3 :: Music Pitch)
          d'  = min ((abs.dur) m0) ((abs.dur) m2)
          m0' = takeM d' (repeatM m0)
          m2' = takeM d' (repeatM m2)

prop_Axiom_11_3_8_weak p c m0 m1 m3 =
    musEquiv p c ((m0 :+: m1) :=: (m2 :+: m3)) ((m0 :=: m2) :+: (m1 :=: m3))
    where types = (p :: PMap Note1, c :: Context Note1, m0 :: Music Pitch, 
                   m1 :: Music Pitch, m2 :: Music Pitch, m3 :: Music Pitch)
          m2 = m0 -- Until dur is fixed, this is the only way to guarantee that dur m0 == dur m2.