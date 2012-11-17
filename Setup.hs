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
