module Main where
import Control.Monad
import System.Console.ANSI
import System.IO
import Data.IORef
import Test.QuickCheck
import Text.Printf
import EuterpeaTests
import Control.Concurrent

totalTests = 1000

-- Thank you, Rosetta Code!
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr str
    setSGR [SetColor Foreground Vivid White, SetColor Background Dull Black]
    putStrLn ""

runTest :: MVar (String, Result) -> (String, IO Result) -> IO ()
runTest result (s, a) = do
    res <- a
    putMVar result (s, res)

printResults :: Int -> MVar (String, Result) -> Handle -> IORef Int -> IO ()
printResults n result log failsR = replicateM_ n $ do
    (s, res) <- takeMVar result
    printf ("%-" ++ show (1 + maximum (map length ((fst . unzip) tests))) ++ "s ") s
    case res of
        Failure _ _ _ _ _ True _ _ -> colorStrLn Vivid Yellow Dull Black "Interrupted"
        Failure _ _ _ _ r _ _ o -> do
            hPutStrLn log $ s ++ ":\n" ++ o ++ "\n"
            colorStrLn Vivid Red Dull Black r
            atomicModifyIORef failsR (\x -> (x+1, ()))
        _ -> colorStrLn Vivid Green Dull Black $ "Passed " ++ show totalTests ++ " trials"

main :: IO ()
main = do
    log <- openFile "error.log" WriteMode
    failsR <- newIORef 0
    result <- newEmptyMVar

    mapM_ (forkIO . runTest result) tests
    printResults (length tests) result log failsR 

    fails <- readIORef failsR
    case fails of
        0 -> putStrLn "*** All tests passed!"
        _ -> putStrLn $ "+++ " ++ show fails ++ " of " ++ show (length tests) ++ " tests failed. See error.log for details."
    hClose log
    return ()

tests = [("AbsPitch_Pitch",            quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_AbsPitch_Pitch),
         ("Trans_Composition",         quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Trans_Composition),
         ("Retro_Composition",         quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Retro_Composition),
         ("Invert_Composition",        quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Invert_Composition),
         ("RetroInvert_Composition",   quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_RetroInvert_Composition),
         ("Dur_Times_Composition",     quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Dur_Times_Composition),
         ("Dur_Take_Composition",      quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Dur_Take_Composition),
         ("Mmap_Id",                   quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Mmap_Id),
         ("Mmap_Function_Composition", quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Mmap_Function_Composition),
         ("TimesM_Seq",                quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_TimesM_Seq),
         ("Mfold_Identity",            quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Mfold_Identity),
         ("revM_SelfInverting",        quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_revM_SelfInverting),
         ("revM_DurationPreserving",   quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_revM_DurationPreserving),
         ("Perf_Id",                   quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Perf_Id),
         ("Axiom_11_2_1",              quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_2_1),
         ("Axiom_11_2_2",              quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_2_2),
         ("Axiom_11_2_3",              quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_2_3),
         ("Theorem_11_2_1",            quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Theorem_11_2_1),
         ("Axiom_11_3_1a",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_1a),
         ("Axiom_11_3_1b",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_1b),
         ("Axiom_11_3_2a",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_2a),
         ("Axiom_11_3_2b",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_2b),
         ("Axiom_11_3_2c",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_2c),
         ("Axiom_11_3_3a",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_3a),
         ("Axiom_11_3_3b",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_3b),
         ("Axiom_11_3_3c",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_3c),
         ("Axiom_11_3_3d",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_3d),
         ("Axiom_11_3_4a",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_4a),
         ("Axiom_11_3_4b",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_4b),
         ("Axiom_11_3_5",              quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_5),
         ("Axiom_11_3_6a",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_6a),
         ("Axiom_11_3_6b",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_6b),
         ("Axiom_11_3_6c",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_6c),
         ("Axiom_11_3_6d",             quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_6d),
         ("Axiom_11_3_8",              quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_Axiom_11_3_8)]

