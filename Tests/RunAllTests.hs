module Main where
import System.IO
import Data.IORef
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import EuterpeaInstances
import EuterpeaTests
import System.Console.ANSI
import Euterpea hiding (Color, Red, Black, White, Green)


-- Thank you, Rosetta Code!
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr str
    setSGR [SetColor Foreground Vivid White, SetColor Background Dull Black]
    putStrLn ""
main = do
    log <- openFile "error.log" WriteMode
    failsR <- newIORef 0
    putStr "Testing AbsPitch_Pitch...            "
    r_AbsPitch_Pitch <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_AbsPitch_Pitch
    case r_AbsPitch_Pitch of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: AbsPitch_Pitch:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Trans_Composition...         "
    r_Trans_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Trans_Composition
    case r_Trans_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Trans_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Retro_Composition...         "
    r_Retro_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Retro_Composition
    case r_Retro_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Retro_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Invert_Composition...        "
    r_Invert_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Invert_Composition
    case r_Invert_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Invert_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing RetroInvert_Composition...   "
    r_RetroInvert_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_RetroInvert_Composition
    case r_RetroInvert_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: RetroInvert_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Dur_Times_Composition...     "
    r_Dur_Times_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Dur_Times_Composition
    case r_Dur_Times_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Dur_Times_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Dur_Take_Composition...      "
    r_Dur_Take_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Dur_Take_Composition
    case r_Dur_Take_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Dur_Take_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Mmap_Id...                   "
    r_Mmap_Id <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Mmap_Id
    case r_Mmap_Id of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Mmap_Id:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Mmap_Function_Composition... "
    r_Mmap_Function_Composition <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Mmap_Function_Composition
    case r_Mmap_Function_Composition of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Mmap_Function_Composition:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing TimesM_Seq...                "
    r_TimesM_Seq <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_TimesM_Seq
    case r_TimesM_Seq of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: TimesM_Seq:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Mfold_Identity...            "
    r_Mfold_Identity <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Mfold_Identity
    case r_Mfold_Identity of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Mfold_Identity:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing revM_SelfInverting...        "
    r_revM_SelfInverting <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_revM_SelfInverting
    case r_revM_SelfInverting of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: revM_SelfInverting:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing revM_DurationPreserving...   "
    r_revM_DurationPreserving <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_revM_DurationPreserving
    case r_revM_DurationPreserving of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: revM_DurationPreserving:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Perf_Id...                   "
    r_Perf_Id <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Perf_Id
    case r_Perf_Id of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Perf_Id:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_2_1...              "
    r_Axiom_11_2_1 <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_2_1
    case r_Axiom_11_2_1 of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_2_1:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_2_2...              "
    r_Axiom_11_2_2 <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_2_2
    case r_Axiom_11_2_2 of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_2_2:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_2_3...              "
    r_Axiom_11_2_3 <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_2_3
    case r_Axiom_11_2_3 of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_2_3:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Theorem_11_2_1...            "
    r_Theorem_11_2_1 <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Theorem_11_2_1
    case r_Theorem_11_2_1 of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Theorem_11_2_1:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_1a...             "
    r_Axiom_11_3_1a <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_1a
    case r_Axiom_11_3_1a of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_1a:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_1b...             "
    r_Axiom_11_3_1b <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_1b
    case r_Axiom_11_3_1b of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_1b:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_2a...             "
    r_Axiom_11_3_2a <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_2a
    case r_Axiom_11_3_2a of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_2a:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_2b...             "
    r_Axiom_11_3_2b <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_2b
    case r_Axiom_11_3_2b of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_2b:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_2c...             "
    r_Axiom_11_3_2c <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_2c
    case r_Axiom_11_3_2c of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_2c:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_3a...             "
    r_Axiom_11_3_3a <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_3a
    case r_Axiom_11_3_3a of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_3a:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_3b...             "
    r_Axiom_11_3_3b <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_3b
    case r_Axiom_11_3_3b of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_3b:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_3c...             "
    r_Axiom_11_3_3c <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_3c
    case r_Axiom_11_3_3c of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_3c:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_3d...             "
    r_Axiom_11_3_3d <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_3d
    case r_Axiom_11_3_3d of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_3d:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_4a...             "
    r_Axiom_11_3_4a <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_4a
    case r_Axiom_11_3_4a of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_4a:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_4b...             "
    r_Axiom_11_3_4b <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_4b
    case r_Axiom_11_3_4b of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_4b:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_5...              "
    r_Axiom_11_3_5 <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_5
    case r_Axiom_11_3_5 of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_5:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_6a...             "
    r_Axiom_11_3_6a <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_6a
    case r_Axiom_11_3_6a of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_6a:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_6b...             "
    r_Axiom_11_3_6b <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_6b
    case r_Axiom_11_3_6b of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_6b:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_6c...             "
    r_Axiom_11_3_6c <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_6c
    case r_Axiom_11_3_6c of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_6c:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_6d...             "
    r_Axiom_11_3_6d <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_6d
    case r_Axiom_11_3_6d of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_6d:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    putStr "Testing Axiom_11_3_8...              "
    r_Axiom_11_3_8 <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = 1000 }) prop_Axiom_11_3_8
    case r_Axiom_11_3_8 of
        Failure _ _ _ _ r _ o -> do
            hPutStrLn log "Test: Axiom_11_3_8:"
            hPutStr   log o
            hPutStrLn log ""
            colorStrLn Vivid Red Dull Black $ "Failure: " ++ show r
            modifyIORef failsR (+1)
        otherwise -> colorStrLn Vivid Green Dull Black "Success: Completed 1000 trials!"

    fails <- readIORef failsR
    case fails of
        0 -> putStrLn "*** All tests passed!"
        _ -> putStrLn $ "+++ " ++ show fails ++ " of 35 tests failed. See error.log for details."
    hClose log
    return ()

