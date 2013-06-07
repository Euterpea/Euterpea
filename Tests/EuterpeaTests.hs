module EuterpeaTests where
import Control.Monad
import Text.Show.Functions
import Test.QuickCheck
import EuterpeaInstances
import Euterpea
infixr 4 =~=

-- Musical equivalence for Pitches, taken from a restriction of the definition from Chapter 11 of HSoM
-- Will replace with arbitrary pmaps and contexts once the axioms hold for the default set

-- Currently suspect that perf is bugged.

(=~=) :: Music Pitch -> Music Pitch -> Bool
mp1 =~= mp2 = perf defPMap defCon (toMusic1 mp1) == perf defPMap defCon (toMusic1 mp2)

-- Tests for Euterpea.Music.Note.Music

prop_AbsPitch_Pitch ap = absPitch (pitch ap) == ap
    where types = ap :: AbsPitch

prop_Trans_Composition i j p = trans i (trans j p) == trans (i+j) p
    where types = (i :: Int, j :: Int, p :: Pitch)

-- Tests for Euterpea.Music.Note.MoreMusic

prop_Retro_Composition = forAll musicPitchLines $
    \line -> (retro.retro) line == line

prop_Invert_Composition = forAll musicPitchLines $
    \line -> (invert.invert) line == line

prop_RetroInvert_Composition = forAll musicPitchLines $
    \line -> (retroInvert.invertRetro) line == line

prop_Dur_Times_Composition m = forAll positiveIntegers $ \n ->
    dur (timesM n m) == toRational n * dur m
    where types = m :: Music Pitch

prop_Dur_Take_Composition m = forAll (liftM toRational (choose (0.0, fromRational (dur m)) :: Gen Double)) $ \d ->
    dur (takeM d m) == d
    where types = m :: Music Pitch

prop_Mmap_Id m = mMap id m == m
    where types = m :: Music Pitch

prop_Mmap_Function_Composition f g m = mMap f (mMap g m) == mMap (f.g) m
    where types = (f :: Pitch -> Pitch, g :: Pitch -> Pitch, m :: Music Pitch)

prop_TimesM_Seq m =
    forAll positiveIntegers $ \a ->
    forAll positiveIntegers $ \b ->
        timesM a m :+: timesM b m =~= timesM (a+b) m
    where types = m :: Music Pitch

prop_Mfold_Identity m = mFold Prim (:+:) (:=:) Modify m == m
    where types = m :: Music Pitch

prop_revM_SelfInverting m = revM (revM m) =~= m
    where types = m :: Music Pitch

-- Tests for Euterpea.Music.Note.Performance

prop_Perf_Id pmap c m = perf pmap c m == (perform pmap c m, dur m)
    where types = (pmap :: PMap Note1, c :: Context Note1, m :: Music Note1)

-- Verification of Axioms in HSoM

prop_Axiom_11_2_1 m = 
    forAll tempos $ \d1 ->
    forAll tempos $ \d2 ->
        tempo d1 (tempo d2 m) =~= tempo (d1*d2) m
    where types = m :: Music Pitch

prop_Axiom_11_2_2 m1 m2 = 
    forAll tempos $ \r ->
        tempo r (m1 :+: m2) =~= tempo r m1 :+: tempo r m2
    where types = (m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_2_3 m = tempo 1 m =~= m
    where types = m :: Music Pitch

prop_Theorem_11_2_1 m1 m2 = forAll tempos $
    \r -> tempo r m1 :+: m2 =~= tempo r (m1 :+: tempo (1/r) m2)

prop_Axiom_11_3_1a m = 
    forAll tempos $ \r1 ->
    forAll tempos $ \r2 ->
        tempo r1 (tempo r2 m) =~= tempo (r1*r2) m
    where types = m :: Music Pitch

prop_Axiom_11_3_1b ap1 ap2 m = trans ap1 (trans ap2 m) == trans (ap1+ap2) m
    where types = (ap1 :: Int, ap2 :: Int, m :: Pitch)


prop_Axiom_11_3_2a m = 
    forAll tempos $ \r1 ->
    forAll tempos $ \r2 ->
        (tempo r1 . tempo r2) m =~= (tempo r2 . tempo r1) m
    where types = m :: Music Pitch

prop_Axiom_11_3_2b p1 p2 m = (trans p1 . trans p2) m == (trans p2 . trans p1) m
    where types = (p1 :: Int, p2 :: Int, m :: Pitch)

{- tempo and trans are incompatible. I assume you meant transpose? -}

prop_Axiom_11_3_2c p m = 
    forAll tempos $ \r1 ->
        (tempo r1 . transpose p) m =~= (transpose p . tempo r1) m
    where types = m :: Music Pitch

prop_Axiom_11_3_3a m1 m2 = 
    forAll tempos $ \r ->
        tempo r (m1 :+: m2) =~= tempo r m1 :+: tempo r m2
    where types = (m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_3b m1 m2 = 
    forAll tempos $ \r ->
        tempo r (m1 :=: m2) =~= tempo r m1 :=: tempo r m2
    where types = (m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_3c r m1 m2 = transpose r (m1 :+: m2) =~= transpose r m1 :+: transpose r m2
    where types = (r :: AbsPitch, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_3d r m1 m2 = transpose r (m1 :=: m2) =~= transpose r m1 :=: transpose r m2
    where types = (r :: AbsPitch, m1 :: Music Pitch, m2 :: Music Pitch)


prop_Axiom_11_3_4a m0 m1 m2 = m0 :+: (m1 :+: m2) =~= (m0 :+: m1) :+: m2
    where types = (m0 :: Music Pitch, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_4b m0 m1 m2 = m0 :=: (m1 :=: m2) =~= (m0 :=: m1) :=: m2
    where types = (m0 :: Music Pitch, m1 :: Music Pitch, m2 :: Music Pitch)

prop_Axiom_11_3_5 m0 m1 = m0 :=: m1 =~= m1 :=: m0
    where types = (m0 :: Music Pitch, m1 :: Music Pitch)

prop_Axiom_11_3_6a =
    forAll tempos $ \r ->
        tempo r (rest 0) =~= rest 0

prop_Axiom_11_3_6b p = transpose p (rest 0) =~= rest 0
    where types = p :: AbsPitch

prop_Axiom_11_3_6c m = (m :+: rest 0 =~= m) && (m =~= rest 0 :+: m)
    where types = m :: Music Pitch

prop_Axiom_11_3_6d m = (m :=: rest 0 =~= m) && (m =~= rest 0 :=: m)
    where types = m :: Music Pitch

{- I couldn't make sense of Axiom 11.3.7 -}

prop_Axiom_11_3_8 m0 m1 m2 m3 =
    (m0' :+: m1) :=: (m2' :+: m3) =~= (m0' :=: m2') :+: (m1 :=: m3)
    where types = (m0 :: Music Pitch, m1 :: Music Pitch, m2 :: Music Pitch, m3 :: Music Pitch)
          m0' = takeM (dur m0) (repeatM m0)
          m2' = takeM (dur m0) (repeatM m2)
