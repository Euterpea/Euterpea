{-# LANGUAGE Arrows #-}

module Euterpea.IO.Audio.ImportWav (wavSF) where

import Prelude hiding (init)
import Euterpea.IO.Audio.Basics (outA)
import Euterpea.IO.Audio.Types (AudSF)

import Control.Arrow
import Control.CCA.Types
import Control.CCA.ArrowP

import Codec.Wav
import Data.Array.Unboxed (elems)
import Data.Audio
import Data.Int (Int32)



-- | Takes a file name and returns a tuple of the samples and the sample rate.
wavToList :: FilePath -> IO ([Double], Int)
wavToList filename = do
    result <- importFile filename
    let (sr,dat) = case result of
                    Left err -> error err
                    Right (Audio sr cn d) -> (sr, d) :: (Int, SampleData Int32)
    let ret = map toSample (elems dat)
    return (ret, sr)

listArrow :: ArrowInit a => b -> [b] -> a () b
listArrow nil binit = proc _ -> do
    rec bs <- init binit -< drop 1 bs
    outA -< if null bs then nil else head bs

-- | This function accepts a file path that points to a mono wav file 
--   encoded at AudRate and returns an AudSF that plays that wav file.
wavSF :: FilePath -> IO (AudSF () Double)
wavSF filename = do
    (lst, sr) <- wavToList filename
    return $ listArrow 0 lst

-- | This function accepts a file path that points to a steroe wav file 
--   encoded at AudRate and returns an AudSF that plays that wav file.
wavSFStereo :: FilePath -> IO (AudSF () (Double,Double))
wavSFStereo filename = do
    (lst, sr) <- wavToList filename
    return $ listArrow (0,0) (tupleList lst)
    where
      tupleList :: [a] -> [(a,a)]
      tupleList [] = []
      tupleList [x] = []
      tupleList (x:x':xs) = (x, x') : tupleList xs
