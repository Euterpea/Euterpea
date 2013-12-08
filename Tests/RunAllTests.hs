module Main where

import AllTests
import Control.Concurrent
import Control.Monad (replicateM_)
import Data.IORef
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO
import Test.QuickCheck
import Text.Printf

totalTests :: Int
totalTests = 1000

-- Thank you, Rosetta Code!
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr str
    setSGR [SetColor Foreground Dull White, SetColor Background Dull Black]
    putStrLn ""

runTest :: MVar (String, Result) -> (String, Property) -> IO ()
runTest result (s, p) = do
    res <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) p
    putMVar result (s, res)

printResults :: Int -> MVar (String, Result) -> Handle -> IORef Int -> IO ()
printResults n result logH failsR = replicateM_ n $ do
    (s, res) <- takeMVar result
    _ <- printf ("%-" ++ show (1 + maximum (map length ((fst . unzip) qctests))) ++ "s ") s
    case res of
        Failure _ _ _ _ _ True _ _ -> colorStrLn Vivid Yellow Dull Black "Interrupted"
        Failure _ _ _ _ r _ _ o -> do
            hPutStrLn logH $ s ++ ":\n" ++ o ++ "\n"
            colorStrLn Vivid Red Dull Black r
            atomicModifyIORef failsR (\x -> (x+1, ()))
        _ -> colorStrLn Vivid Green Dull Black $ "Passed " ++ show totalTests ++ " trials"

main :: IO ()
main = do
    logH <- openFile "error.log" WriteMode
    failsR <- newIORef 0
    result <- newEmptyMVar

    mapM_ (forkIO . runTest result) qctests
    printResults (length qctests) result logH failsR 

    fails <- readIORef failsR
    hClose logH
    case fails of
        0 -> putStrLn "*** All tests passed!"
        _ -> putStrLn ("+++ " ++ show fails ++ " of " ++ show (length qctests) 
                    ++ " tests failed. See error.log for details.") 
             >> exitFailure
