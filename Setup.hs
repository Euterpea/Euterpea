import Distribution.Simple
main = defaultMain

-- January 18, 2014
-- The following setup script uses the CCA preprocessor (ccap) to preprocess 
-- certain *.as files in the Euterpea code base.  As of January 18, 2014, only 
-- one file is being preprocessed in this way (Euterpea.IO.Audio.Basics), and 
-- as some users have had difficulty with installations due to this 
-- preprocessing step, we are removing it from the installation procedure.
-- 
-- Now, to process *.as files, one can directly use the ArrowWrap module in 
-- Euterpea.  In ArrowWrap, all files to be preprocessed must be declared in 
-- the list called fileList.  Then, simply run main.
-- 
-- If this preprocessor is going to be reenabled, or if ArrowWrap is going 
-- to be used, one must either add haskell-src-exts >= 1.14.0 to the cabal 
-- build-depends or just install it directly.
{-
module Main (main) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Utils
import System.Exit

import ArrowWrap

findArrowP verbosity = do
  a <- findProgramLocation verbosity "ccap"
  case a of 
    Nothing -> error "Preprocessor ccap not found. Please make sure the \
                     \CCA library is already installed, and ccap is in \
                     \your PATH environment."
    Just p  -> return p

ppArrow bi lbi = PreProcessor {
    platformIndependent = True,
    runPreProcessor = 
      mkSimplePreProcessor $ \inFile outFile verbosity ->
        do info verbosity (inFile ++ " has been preprocessed to " ++ outFile)
           arrowp <- findArrowP verbosity
           runArrowP arrowp inFile outFile
  }

myHooks = simpleUserHooks 
            { hookedPreProcessors = ("as", ppArrow) : knownSuffixHandlers }

main :: IO ()
main = defaultMainWithHooks myHooks
-}