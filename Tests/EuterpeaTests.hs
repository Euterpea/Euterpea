{-# OPTIONS -XTypeSynonymInstances #-}
{-# OPTIONS -XFlexibleInstances #-}
module EuterpeaTests where
import Control.Monad
import Test.QuickCheck
import Euterpea

-- Instances and generators for Music-related types and data from Music.hs

genPositiveRational :: Gen Rational
genPositiveRational = liftM abs (arbitrary :: Gen Rational)

genOctave :: Gen Octave
genOctave = choose (0,9) -- The tenth octave isn't full.

genPitch :: Gen Pitch
genPitch = do
    pc <- arbitrary
    oc <- genOctave
    return (pc,oc)

genDur :: Gen Dur
genDur = do
    num <- choose (0,64) :: Gen Integer
    den <- choose (1,16) :: Gen Integer
    return (toRational num / toRational den)

instance Bounded PitchClass where
    minBound = Cff
    maxBound = Bss

instance Arbitrary PitchClass where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Primitive Pitch) where
    arbitrary = oneof [
        liftM2 Note genDur genPitch,
        liftM  Rest genDur]

instance Arbitrary (Music Pitch) where
    arbitrary = song
        where song = sized song'
              song' 0 = liftM Prim arbitrary
              song' n = let submp = song' (n `div` 2)
                         in oneof [ liftM Prim arbitrary,
                                    liftM2 (:+:) submp submp,
                                    liftM2 (:=:) submp submp,
                                    liftM2 Modify arbitrary submp]

instance Arbitrary Control where
    arbitrary = oneof [
        liftM Tempo genPositiveRational,
        liftM Transpose (choose (0,127)),
        liftM Instrument arbitrary,
        liftM Phrase (vector 10),
        liftM Player arbitrary,
        liftM2 KeySig arbitrary arbitrary]

instance Enum Mode where
    toEnum 0 = Major
    toEnum 1 = Minor
    fromEnum Major = 0
    fromEnum Minor = 1

instance Bounded Mode where
    minBound = Major
    maxBound = Minor

instance Arbitrary Mode where
    arbitrary = arbitraryBoundedEnum

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
        liftM Accent genPositiveRational,
        liftM Crescendo genPositiveRational,
        liftM Diminuendo genPositiveRational,
        liftM StdLoudness arbitrary,
        liftM Loudness genPositiveRational]

instance Bounded StdLoudness where
    minBound = PPP
    maxBound = FFF

instance Arbitrary StdLoudness where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Tempo where
    arbitrary = oneof [
        liftM Ritardando genPositiveRational,
        liftM Accelerando genPositiveRational ]

instance Arbitrary Articulation where
    arbitrary = oneof [
        liftM Staccato genPositiveRational,
        liftM Legato   genPositiveRational,
        liftM Slurred  genPositiveRational,
        liftM (as !!) (choose (0, length as - 1))]
        where as = [ Tenuto, Marcato, Pedal, Fermata, FermataDown, Breath, 
                     DownBow, UpBow, Harmonic, Pizzicato, LeftPizz, 
                     BartokPizz, Swell, Wedge, Thumb, Stopped ]

instance Arbitrary Ornament where
    arbitrary = oneof [
        liftM Instruction arbitrary,
        liftM Head arbitrary,
        liftM DiatonicTrans (liftM (\x -> x `mod` 12) arbitrary),
        liftM (os !!) (choose (0, length os - 1))]
        where os = [ Trill, Mordent, InvMordent, DoubleMordent, Turn,
                     TrilledTurn, ShortTrill, Arpeggio, ArpeggioUp,
                     ArpeggioDown ]

instance Arbitrary NoteHead where
    arbitrary = liftM (ns !!) (choose (0, length ns - 1))
        where ns = [ DiamondHead, SquareHead, XHead, TriangleHead,
                     TremoloHead, SlashHead, ArtHarmonic, NoHead ]

-- Instances and generators for Music-related types and data from MoreMusic.hs
