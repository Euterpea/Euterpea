module ArrowWrap where

import qualified Data.Generics as G

import Language.Haskell.Exts
import Data.List
import Data.Maybe
import System.Environment
import System.Process

import Distribution.Simple.Program (findProgramLocation)
import Distribution.Verbosity (silent)


isTyVar (TyVar _) = True
isTyVar _         = False

insertForall t@(TyForall Nothing ctx ty) = 
  TyForall (Just frAlls) ctx ty
    where frAlls = map mkTyVarBind vars
          vars   = nub (G.listify isTyVar t)
insertForall x = x

mkTyVarBind (TyVar x) = UnkindedVar x

parseMode fn = ParseMode 
     { parseFilename         = fn,
       baseLanguage          = Haskell2010,
       extensions            = fmap EnableExtension [ 
                                 MultiParamTypeClasses,
                                 FlexibleContexts,
                                 TemplateHaskell,
                                 ExistentialQuantification,
                                 BangPatterns,
                                 FunctionalDependencies,
                                 Rank2Types ] ,
       ignoreLanguagePragmas = False,
       ignoreLinePragmas     = False,
       fixities              = Just preludeFixities }

-- fixme: the line pragmas are actually based on the output of arrowp
ppMode = defaultMode { linePragmas = False }



parseArrowPOutput filename str = 
  let pm          = parseMode filename 
      parseResult = parseFileContentsWithMode pm str
  in case parseResult of
            ParseOk m           -> G.everywhere (G.mkT insertForall) m
            ParseFailed loc err -> error $ 
                        "Parse error: " ++ show loc ++ ": " ++ show err

mkPragma exts = "{-# LANGUAGE " ++ 
                intercalate ", " (map (show . enabled) exts) ++
                " #-}"
  where enabled (EnableExtension ext) = ext

{-
Run arrowp, parse result (since haskell-src-exts doesn't handle 
arrow syntax as of July 2009).  
Insert "forall" into type signatures; prettyprint result
-}

runArrowP arrowp inFile outFile = do
  result <- readProcess arrowp [inFile] []
  orig   <- readFile inFile
  let (_, exts) = fromJust (readExtensions orig)
      parse  = parseArrowPOutput inFile result
      pragma = mkPragma exts
  writeFile outFile $ pragma ++ "\n" ++ prettyPrintWithMode ppMode parse


-- The list of files to be processed, given as a pair of input name and output name.
-- This is used when using main, but not when using the cabal preprocessor.
fileList = [("Euterpea/IO/Audio/Basics.as", "Euterpea/IO/Audio/Basics.hs")]

-- The main function that, when this is not being used as a preprocessor 
-- with cabal, should be run to do the processing.
main = do
    arrowp <- findArrowP silent
    mapM_ (f arrowp) fileList
  where
  f arrowp (inFile, outFile) = do
    runArrowP arrowp inFile outFile
    putStrLn $ inFile ++ " has been preprocessed to " ++ outFile

-- Copied from Setup.hs so it can be used in the main method here.
findArrowP verbosity = do
  a <- findProgramLocation verbosity "ccap"
  case a of 
    Nothing -> error "Preprocessor ccap not found. Please make sure the \
                     \CCA library is already installed, and ccap is in \
                     \your PATH environment."
    Just p  -> return p
