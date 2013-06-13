import os, shutil, subprocess, sys, multiprocessing

totalTrials = 0
try:
    totalTrials = int(sys.argv[1])
except:
    totalTrials = 1000

num_threads = 1
try:
    num_threads = int(sys.argv[2])
except:
    num_threads = multiprocessing.cpu_count() + 1

content = open("EuterpeaTests.hs").readlines()
tests = []

for line in content:
    if line.startswith("prop_"):
        tests.append(line[5:line.index(' ')])

def getSpacing(test):
    width = max(map(len,tests))
    return (" " * (width - len(test)))

print "Generating test script..."

hs = open('RunAllTests.hs','w')

hs.write("""module Main where
import Control.Monad
import System.Console.ANSI
import System.IO
import Data.IORef
import Test.QuickCheck
import Text.Printf
import EuterpeaTests
import Control.Concurrent

totalTests = {}

-- Thank you, Rosetta Code!
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr str
    setSGR [SetColor Foreground Dull White, SetColor Background Dull Black]
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
            hPutStrLn log $ s ++ ":\\n" ++ o ++ "\\n"
            colorStrLn Vivid Red Dull Black r
            atomicModifyIORef failsR (\\x -> (x+1, ()))
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

tests = [""".format(totalTrials))

testsStr = ""

for test in tests:
    testsStr += "         (\"" + test + "\", " + getSpacing(test) + "quickCheckWithResult (stdArgs { chatty = False, maxSuccess = totalTests }) prop_" + test + "),\n"

hs.write(testsStr[len("tests = ["):-2] + "]\n\n")
hs.close()

print "Running tests on {} thread(s)...".format(num_threads)

subprocess.call(["ghc", "-e", "main", "RunAllTests.hs", "+RTS", "-N" + str(num_threads)])
