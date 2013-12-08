{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module EuterpeaInstances where
import Control.Monad
import Test.QuickCheck
import Euterpea hiding (run, den, as, cs, d)

-- Specialized generators for tests that must work under particular constraints.

positiveIntegers :: Gen Int
positiveIntegers = liftM abs (arbitrary :: Gen Int)

smallIntegers :: Gen Int
smallIntegers = choose (0,50)

positiveRationals :: Gen Rational
positiveRationals = liftM ((+1).abs) (arbitrary :: Gen Rational)

unitBoundedRationals :: Gen Rational
unitBoundedRationals = do
    num <- choose (-1000,1000) :: Gen Integer
    return (toRational num / 1000)

octaves :: Gen Octave
octaves = choose (0,10)

tempos :: Gen Dur
tempos = do
    num <- positiveRationals
    den <- positiveRationals
    return (num/den)
    
genVolume :: Gen Volume
genVolume = choose (0,127)

genAbsPitch :: Gen AbsPitch
genAbsPitch = choose (0,127)

musicPitchLines :: Gen (Music Pitch)
musicPitchLines = liftM (line . map Prim) (listOf (arbitrary :: Gen (Primitive Pitch)))

-- Instances for Music-related types and data from Euterpea.Music.Note.Music

instance Arbitrary Dur where
    arbitrary = do
        num <- choose (0,64) :: Gen Integer
        den <- choose (1,16) :: Gen Integer
        return (toRational num / toRational den)

instance Bounded PitchClass where
    minBound = Cff
    maxBound = Bss

instance Arbitrary PitchClass where
    arbitrary = arbitraryBoundedEnum

instance CoArbitrary PitchClass where
    coarbitrary = variant . fromEnum

instance Arbitrary Pitch where
    arbitrary = do
        pc <- arbitrary
        oc <- octaves
        return (pc,oc)

instance Arbitrary a => Arbitrary (Primitive a) where
    arbitrary = oneof [
        liftM2 Note arbitrary arbitrary,
        liftM  Rest arbitrary]

instance (Arbitrary a, Eq a) => Arbitrary (Music a) where
    arbitrary = song
        where song = sized song'
              song' 0 = liftM Prim arbitrary
              song' n = let submp = song' (n `div` 2)
                         in oneof [ liftM  Prim arbitrary,
                                    liftM2 (:+:) submp submp,
                                    liftM2 (:=:) submp submp,
                                    liftM2 Modify arbitrary submp]

    shrink m = [shrink' m | m /= m']
        where m' = shrink' m

              isUseful (Orn _) = False
              isUseful _ = True

              shrink' (mp1 :+: mp2)
                | dur mp1 == 0 && dur mp2 == 0 = rest 0
                | dur mp1 == 0 = shrink' mp2
                | dur mp2 == 0 = shrink' mp1
                | otherwise    = shrink' mp1 :+: shrink' mp2
              shrink' (mp1 :=: mp2)
                | dur mp1 == 0 && dur mp2 == 0 = rest 0
                | dur mp1 == 0 = shrink' mp2
                | dur mp2 == 0 = shrink' mp1
                | otherwise    = shrink' mp1 :=: shrink' mp2
              shrink' (Modify (Phrase (_:cs)) mp) = Modify (Phrase (filter isUseful cs))
                                                    (if dur mp == 0 then rest 0 else shrink' mp)
              shrink' (Modify (Transpose 0) mp) = if dur mp == 0 then rest 0 else shrink' mp
              shrink' (Modify (Instrument _) mp) = if dur mp == 0 then rest 0 else shrink' mp
              shrink' (Modify (Player pl) mp)
                | dur mp == 0   = rest 0
                | pl == "Fancy" = Modify (Player pl) (shrink' mp)
                | otherwise     = shrink' mp
              shrink' (Modify p mp) = Modify p (if dur mp == 0 then rest 0 else shrink' mp)
              shrink' p             = p


instance CoArbitrary (Music Pitch) where
    coarbitrary (Prim (Note d p)) = variant 0 . coarbitrary d . coarbitrary p
    coarbitrary (Prim (Rest d)) = variant 1 . coarbitrary d
    coarbitrary (mp1 :+: mp2) = variant 2 . coarbitrary mp1 . coarbitrary mp2
    coarbitrary (mp1 :=: mp2) = variant 3 . coarbitrary mp1 . coarbitrary mp2
    coarbitrary (Modify _ mp) = variant 4 . coarbitrary mp -- TODO: Implement CoArbitrary for Control

instance Arbitrary Control where
    arbitrary = oneof [
        liftM Tempo tempos,
        liftM Transpose (choose (0,127)),
        liftM Instrument arbitrary,
        liftM Phrase (vector 10),
        liftM Player arbitrary,
        liftM2 KeySig arbitrary arbitrary]

instance Arbitrary Mode where
    arbitrary = elements [Major, Minor]

instance Arbitrary InstrumentName where
    arbitrary = oneof [
        liftM Custom arbitrary,
        liftM (is !!) (choose (0, length is - 1))]
        where is = [ AcousticGrandPiano, BrightAcousticPiano, ElectricGrandPiano, HonkyTonkPiano,
                     RhodesPiano, ChorusedPiano, Harpsichord, Clavinet, Celesta,  Glockenspiel,
                     MusicBox, Vibraphone, Marimba, Xylophone, TubularBells, Dulcimer,
                     HammondOrgan, PercussiveOrgan, RockOrgan, ChurchOrgan, ReedOrgan, Accordion,
                     Harmonica, TangoAccordion, AcousticGuitarNylon, AcousticGuitarSteel,
                     ElectricGuitarJazz, ElectricGuitarClean , ElectricGuitarMuted, OverdrivenGuitar,
                     DistortionGuitar, GuitarHarmonics, AcousticBass, ElectricBassFingered,
                     ElectricBassPicked, FretlessBass, SlapBass1, SlapBass2, SynthBass1, SynthBass2,
                     Violin, Viola, Cello, Contrabass, TremoloStrings, PizzicatoStrings, OrchestralHarp,
                     StringEnsemble1, StringEnsemble2, SynthStrings1, SynthStrings2, ChoirAahs, VoiceOohs,
                     SynthVoice, OrchestraHit, Trumpet, Trombone, Tuba, MutedTrumpet, FrenchHorn,
                     BrassSection, SynthBrass1, SynthBrass2, SopranoSax, AltoSax, TenorSax, BaritoneSax,
                     Oboe, Bassoon, EnglishHorn, Clarinet, Piccolo, Flute, Recorder, PanFlute, BlownBottle,
                     Shakuhachi, Whistle, Ocarina, Lead1Square, Lead2Sawtooth, Lead3Calliope, Lead4Chiff,
                     Lead5Charang, Lead6Voice, Lead7Fifths, Lead8BassLead, Pad1NewAge, Pad2Warm,
                     Pad3Polysynth, Pad4Choir, Pad5Bowed, Pad6Metallic, Pad7Halo, Pad8Sweep, FX1Train,
                     FX2Soundtrack, FX3Crystal, FX4Atmosphere, FX5Brightness, FX6Goblins, FX7Echoes,
                     FX8SciFi, Sitar, Banjo, Shamisen, Koto, Kalimba, Bagpipe, Fiddle, Shanai, TinkleBell,
                     Agogo, SteelDrums, Woodblock, TaikoDrum, MelodicDrum, SynthDrum, ReverseCymbal,
                     GuitarFretNoise, BreathNoise, Seashore, BirdTweet, TelephoneRing, Helicopter,
                     Applause, Gunshot, Percussion]

instance Arbitrary PhraseAttribute where
    arbitrary = oneof [
        liftM Dyn arbitrary,
        liftM Tmp arbitrary,
        liftM Art arbitrary,
        liftM Orn arbitrary]

instance Arbitrary Dynamic where
    arbitrary = oneof [
        liftM Accent positiveRationals,
        liftM Crescendo positiveRationals,
        liftM Diminuendo positiveRationals,
        liftM StdLoudness arbitrary,
        liftM Loudness positiveRationals]

instance Bounded StdLoudness where
    minBound = PPP
    maxBound = FFF

instance Arbitrary StdLoudness where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Tempo where
    arbitrary = oneof [
        liftM Ritardando unitBoundedRationals,
        liftM Accelerando unitBoundedRationals ]

instance Arbitrary Articulation where
    arbitrary = oneof [
        liftM Staccato positiveRationals,
        liftM Legato   positiveRationals,
        liftM Slurred  positiveRationals,
        liftM (as !!) (choose (0, length as - 1))]
        where as = [ Tenuto, Marcato, Pedal, Fermata, FermataDown, Breath, 
                     DownBow, UpBow, Harmonic, Pizzicato, LeftPizz, 
                     BartokPizz, Swell, Wedge, Thumb, Stopped ]

instance Arbitrary Ornament where
    arbitrary = oneof [
        liftM Instruction arbitrary,
        liftM Head arbitrary,
        liftM (DiatonicTrans . (`mod` 12)) arbitrary,
        liftM (os !!) (choose (0, length os - 1))]
        where os = [ Trill, Mordent, InvMordent, DoubleMordent, Turn,
                     TrilledTurn, ShortTrill, Arpeggio, ArpeggioUp,
                     ArpeggioDown ]

instance Arbitrary NoteHead where
    arbitrary = liftM (ns !!) (choose (0, length ns - 1))
        where ns = [ DiamondHead, SquareHead, XHead, TriangleHead,
                     TremoloHead, SlashHead, ArtHarmonic, NoHead ]

-- Instances for Music-related types and data from Euterpea.Music.Note.MoreMusic

instance Bounded PercussionSound where
    minBound = AcousticBassDrum
    maxBound = OpenTriangle

instance Arbitrary PercussionSound where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NoteAttribute where
    arbitrary = oneof [
        liftM Volume genVolume,
        liftM Fingering (choose (0,10)),
        liftM Dynamics arbitrary,
        liftM Params (listOf arbitrary)]

-- Instances for Music-related types and data from Euterpea.Music.Note.Performance
-- Where there are defaults in Euterpea, they are being used instead of random instances.

instance Arbitrary Performance where
    arbitrary = listOf arbitrary

instance Arbitrary Event where
    arbitrary = return
        Event `ap` positiveRationals -- eTime
              `ap` arbitrary         -- eInst
              `ap` genAbsPitch       -- ePitch
              `ap` arbitrary         -- eDur
              `ap` genVolume         -- eVol
              `ap` listOf arbitrary  -- eParams

instance Arbitrary (PMap Note1) where
    arbitrary = elements [defPMap]

instance Arbitrary (Context Note1) where
    arbitrary = return 
        Context `ap` positiveRationals -- cTime
                `ap` arbitrary         -- cPlayer
                `ap` arbitrary         -- cInst
                `ap` arbitrary         -- cDur
                `ap` genAbsPitch       -- cPch
                `ap` genVolume         -- cVol
                `ap` arbitrary         -- cKey

{- Players are difficult (if not impossible) to generate since they need to satisfy axioms.
   In my opinion, we should only worry about testing the players we package with Euterpea,
   rather than trying to generate random players. Let users test their own additions.
     -- Alex
   
   When fancyPlayer is in the list below, half the tests fail.
-}
                
instance Arbitrary (Player Note1) where
    arbitrary = elements [defPlayer, fancyPlayer]
