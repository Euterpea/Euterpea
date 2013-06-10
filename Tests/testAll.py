import os, shutil, subprocess, sys

totalTrials = 0
try:
    totalTrials = int(sys.argv[1])
except:
    totalTrials = 100

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

hs.write("module Main where\n")
hs.write("import System.IO\n")
hs.write("import Data.IORef\n")
hs.write("import Text.Show.Functions\n")
hs.write("import Test.QuickCheck\n")
hs.write("import Control.Monad\n")
hs.write("import EuterpeaInstances\n")
hs.write("import EuterpeaTests\n")
hs.write("import System.Console.ANSI\n")
hs.write("import Euterpea hiding (Color, Red, Black, White, Green)\n")
hs.write("\n")
hs.write("""
-- Thank you, Rosetta Code!
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr str
    setSGR [SetColor Foreground Vivid White, SetColor Background Dull Black]
    putStrLn ""
""")
hs.write("main = do\n")
hs.write("    log <- openFile \"error.log\" WriteMode\n")
hs.write("    failsR <- newIORef 0\n")
for test in tests:
    hs.write("    putStr \"Testing " + test + "... " + getSpacing(test) + "\"\n")
    hs.write("    r_" + test + " <- quickCheckWithResult (stdArgs { chatty = False, maxSuccess = " + str(totalTrials) + " }) prop_" + test + "\n")
    hs.write("    case r_" + test + " of\n")
    hs.write("        Failure _ _ _ _ r _ o -> do\n")
    hs.write("            hPutStrLn log \"Test: " + test + ":\"\n")
    hs.write("            hPutStr   log o\n")
    hs.write("            hPutStrLn log \"\"\n")
    hs.write("            colorStrLn Vivid Red Dull Black $ \"Failure: \" ++ show r\n")
    hs.write("            modifyIORef failsR (+1)\n")
    hs.write("        otherwise -> colorStrLn Vivid Green Dull Black \"Success: Completed " + str(totalTrials) + " trials!\"\n\n")
hs.write("    fails <- readIORef failsR\n")
hs.write("    case fails of\n")
hs.write("        0 -> putStrLn \"*** All tests passed!\"\n")
hs.write("        _ -> putStrLn $ \"+++ \" ++ show fails ++ \" of " + str(len(tests)) + " tests failed. See error.log for details.\"\n")
hs.write("    hClose log\n")
hs.write("    return ()\n")

hs.write("\n")
hs.close()

print "Compiling test script..."

subprocess.call(["ghc", "-outputdir", "tmp", "RunAllTests.hs"])
shutil.rmtree("tmp")

print "Running tests..."

subprocess.call(["RunAllTests.exe"])
