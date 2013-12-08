module AllTests (
    qctests
  --, tests
) where

import Test.QuickCheck
import EuterpeaTests

-- I think that ideally we want to use the "detailed" cabal test suite type, 
-- but I can't get it to work.  If I could, then the following four lines 
-- should be uncommented, and the test-suite build-depends should additionally 
-- require cabal-test-quickcheck in place of ansi-terminal (which is then only 
-- necessary when running the tests manually using RunAllTests.main).

--import Distribution.TestSuite.QuickCheck
--
--tests :: IO [Test]
--tests = return $ map (uncurry testProperty) qctests

qctests :: [(String, Property)]
qctests = [("AbsPitch_Pitch",            property prop_AbsPitch_Pitch),
           ("Trans_Composition",         property prop_Trans_Composition),
           ("Retro_Composition",         property prop_Retro_Composition),
           ("Invert_Composition",        property prop_Invert_Composition),
           ("RetroInvert_Composition",   property prop_RetroInvert_Composition),
           ("Dur_Times_Composition",     property prop_Dur_Times_Composition),
           ("Dur_Take_Composition",      property prop_Dur_Take_Composition),
           ("Take_Repeat_Id",            property prop_Take_Repeat_Id),
           ("Mmap_Id",                   property prop_Mmap_Id),
           ("Mmap_Function_Composition", property prop_Mmap_Function_Composition),
           ("TimesM_Seq",                property prop_TimesM_Seq),
           ("Mfold_Identity",            property prop_Mfold_Identity),
           ("revM_SelfInverting",        property prop_revM_SelfInverting),
           ("revM_SelfInverting_weak",   property prop_revM_SelfInverting_weak),
           ("revM_DurationPreserving",   property prop_revM_DurationPreserving),
           ("Perf_Id",                   property prop_Perf_Id),
           ("Axiom_11_2_1",              property prop_Axiom_11_2_1),
           ("Axiom_11_2_2",              property prop_Axiom_11_2_2),
           ("Axiom_11_2_3",              property prop_Axiom_11_2_3),
           ("Theorem_11_2_1",            property prop_Theorem_11_2_1),
           ("Axiom_11_3_1a",             property prop_Axiom_11_3_1a),
           ("Axiom_11_3_1b",             property prop_Axiom_11_3_1b),
           ("Axiom_11_3_2a",             property prop_Axiom_11_3_2a),
           ("Axiom_11_3_2b",             property prop_Axiom_11_3_2b),
           ("Axiom_11_3_2c",             property prop_Axiom_11_3_2c),
           ("Axiom_11_3_3a",             property prop_Axiom_11_3_3a),
           ("Axiom_11_3_3b",             property prop_Axiom_11_3_3b),
           ("Axiom_11_3_3c",             property prop_Axiom_11_3_3c),
           ("Axiom_11_3_3d",             property prop_Axiom_11_3_3d),
           ("Axiom_11_3_4a",             property prop_Axiom_11_3_4a),
           ("Axiom_11_3_4b",             property prop_Axiom_11_3_4b),
           ("Axiom_11_3_5",              property prop_Axiom_11_3_5),
           ("Axiom_11_3_6a",             property prop_Axiom_11_3_6a),
           ("Axiom_11_3_6b",             property prop_Axiom_11_3_6b),
           ("Axiom_11_3_6c",             property prop_Axiom_11_3_6c),
           ("Axiom_11_3_6d",             property prop_Axiom_11_3_6d),
           ("Axiom_11_3_8",              property prop_Axiom_11_3_8),
           ("Axiom_11_3_8_weak",         property prop_Axiom_11_3_8_weak)]
