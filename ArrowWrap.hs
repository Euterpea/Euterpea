module ArrowWrap where

import qualified Data.Generics as G

import Language.Haskell.Exts
import Data.List
import Data.Maybe
import System.Environment
import System.Process


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
       extensions            = [ MultiParamTypeClasses,
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
                concat (intersperse "," (map show exts)) ++
                " #-}"

{-
Run arrowp, parse result (since haskell-src-exts doesn't handle 
arrow syntax as of July 2009).  
Insert "forall" into type signatures; prettyprint result
-}

runArrowP arrowp inFile outFile = do
  result <- readProcess arrowp [inFile] []
  orig   <- readFile inFile
  let exts   = fromJust (readExtensions orig)
      parse  = parseArrowPOutput inFile result
      pragma = mkPragma exts
  writeFile outFile $ pragma ++ "\n" ++ prettyPrintWithMode ppMode parse
