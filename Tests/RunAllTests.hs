module Main where
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import EuterpeaInstances
import EuterpeaTests
import Euterpea

main = do
    putStrLn "Testing AbsPitch_Pitch..."
    quickCheck prop_AbsPitch_Pitch
    putStrLn ""

    putStrLn "Testing Trans_Composition..."
    quickCheck prop_Trans_Composition
    putStrLn ""

    putStrLn "Testing Retro_Composition..."
    quickCheck prop_Retro_Composition
    putStrLn ""

    putStrLn "Testing Invert_Composition..."
    quickCheck prop_Invert_Composition
    putStrLn ""

    putStrLn "Testing RetroInvert_Composition..."
    quickCheck prop_RetroInvert_Composition
    putStrLn ""

    putStrLn "Testing Dur_Times_Composition..."
    quickCheck prop_Dur_Times_Composition
    putStrLn ""

    putStrLn "Testing Dur_Take_Composition..."
    quickCheck prop_Dur_Take_Composition
    putStrLn ""

    putStrLn "Testing Mmap_Id..."
    quickCheck prop_Mmap_Id
    putStrLn ""

    putStrLn "Testing Mmap_Function_Composition..."
    quickCheck prop_Mmap_Function_Composition
    putStrLn ""

    putStrLn "Testing TimesM_Seq..."
    quickCheck prop_TimesM_Seq
    putStrLn ""

    putStrLn "Testing Mfold_Identity..."
    quickCheck prop_Mfold_Identity
    putStrLn ""

    putStrLn "Testing revM_SelfInverting..."
    quickCheck prop_revM_SelfInverting
    putStrLn ""

    putStrLn "Testing Perf_Id..."
    quickCheck prop_Perf_Id
    putStrLn ""

    putStrLn "Testing Axiom_11_2_1..."
    quickCheck prop_Axiom_11_2_1
    putStrLn ""

    putStrLn "Testing Axiom_11_2_2..."
    quickCheck prop_Axiom_11_2_2
    putStrLn ""

    putStrLn "Testing Axiom_11_2_3..."
    quickCheck prop_Axiom_11_2_3
    putStrLn ""

    putStrLn "Testing Theorem_11_2_1..."
    quickCheck prop_Theorem_11_2_1
    putStrLn ""

    putStrLn "Testing Axiom_11_3_1a..."
    quickCheck prop_Axiom_11_3_1a
    putStrLn ""

    putStrLn "Testing Axiom_11_3_1b..."
    quickCheck prop_Axiom_11_3_1b
    putStrLn ""

    putStrLn "Testing Axiom_11_3_2a..."
    quickCheck prop_Axiom_11_3_2a
    putStrLn ""

    putStrLn "Testing Axiom_11_3_2b..."
    quickCheck prop_Axiom_11_3_2b
    putStrLn ""

    putStrLn "Testing Axiom_11_3_2c..."
    quickCheck prop_Axiom_11_3_2c
    putStrLn ""

    putStrLn "Testing Axiom_11_3_3a..."
    quickCheck prop_Axiom_11_3_3a
    putStrLn ""

    putStrLn "Testing Axiom_11_3_3b..."
    quickCheck prop_Axiom_11_3_3b
    putStrLn ""

    putStrLn "Testing Axiom_11_3_3c..."
    quickCheck prop_Axiom_11_3_3c
    putStrLn ""

    putStrLn "Testing Axiom_11_3_3d..."
    quickCheck prop_Axiom_11_3_3d
    putStrLn ""

    putStrLn "Testing Axiom_11_3_4a..."
    quickCheck prop_Axiom_11_3_4a
    putStrLn ""

    putStrLn "Testing Axiom_11_3_4b..."
    quickCheck prop_Axiom_11_3_4b
    putStrLn ""

    putStrLn "Testing Axiom_11_3_5..."
    quickCheck prop_Axiom_11_3_5
    putStrLn ""

    putStrLn "Testing Axiom_11_3_6a..."
    quickCheck prop_Axiom_11_3_6a
    putStrLn ""

    putStrLn "Testing Axiom_11_3_6b..."
    quickCheck prop_Axiom_11_3_6b
    putStrLn ""

    putStrLn "Testing Axiom_11_3_6c..."
    quickCheck prop_Axiom_11_3_6c
    putStrLn ""

    putStrLn "Testing Axiom_11_3_6d..."
    quickCheck prop_Axiom_11_3_6d
    putStrLn ""

    putStrLn "Testing Axiom_11_3_8..."
    quickCheck prop_Axiom_11_3_8
    putStrLn ""

    putStrLn "Done! Press any key to continue."
    a <- getChar
    return ()

