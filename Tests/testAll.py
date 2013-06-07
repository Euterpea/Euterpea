import os, shutil, subprocess

content = open("EuterpeaTests.hs").readlines()
tests = []

for line in content:
    if line.startswith("prop_"):
        tests.append(line[5:line.index(' ')])

print "Generating test script..."

hs = open('test.hs','w')

hs.write("module Main where\n")
hs.write("import Text.Show.Functions\n")
hs.write("import Test.QuickCheck\n")
hs.write("import Control.Monad\n")
hs.write("import EuterpeaInstances\n")
hs.write("import EuterpeaTests\n")
hs.write("import Euterpea\n")
hs.write("\n")
hs.write("main = do\n")
for test in tests:
    hs.write("    putStrLn \"Testing " + test + "...\"\n")
    hs.write("    quickCheck prop_" + test + "\n")
    hs.write("    putStrLn \"\"\n\n")
hs.write("    putStrLn \"Done! Press any key to continue.\"\n")
hs.write("    a <- getChar\n")
hs.write("    return ()\n")

hs.write("\n")
hs.close()

print "Compiling test script..."

subprocess.call(["ghc", "-outputdir", "tmp", "test.hs"])
shutil.rmtree("tmp")
os.unlink("test.hs")

print "Running tests..."

subprocess.call(["test.exe"])
