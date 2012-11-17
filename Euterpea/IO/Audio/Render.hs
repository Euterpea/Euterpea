{-# LANGUAGE Arrows, ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}

-- Render a Music object to a audio signal function that can be further
-- manipulated or saved to a file.  It is channel-agnostic in that it is
-- able to deal with instruments of arbitrary number of channels.

module Euterpea.IO.Audio.Render (
  Instr, InstrMap, renderSF, 
) where

import Control.Arrow
import Control.CCA.Types
import Control.CCA.ArrowP
import Control.SF.SF

import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.Music.Note.Performance
import Euterpea.IO.Audio.Basics
import Euterpea.IO.Audio.Types

import Data.List hiding (init)
import qualified Data.IntMap as M
import Data.Ord (comparing)

import Prelude hiding (init)

-- Every instrument is a function that takes a duration, absolute
-- pitch, volume, and a list of parameters (Doubles).  What the function 
-- actually returns is implementation independent.
type Instr a = Dur -> AbsPitch -> Volume -> [Double] -> a

type InstrMap a = [(InstrumentName, Instr a)]

lookupInstr :: InstrumentName -> InstrMap a -> Instr a
lookupInstr ins im =
    case lookup ins im of
      Just i -> i
      Nothing -> error $ "Instrument " ++ show ins ++ 
                 " does not have a matching Instr in the supplied InstrMap."

-- Each note in a Performance is tagged with a unique NoteId, which
-- helps us keep track of the signal function associated with a note.
type NoteId = Int

-- In this particular implementation, 'a' is the signal function that
-- plays the given note.
data NoteEvt a = NoteOn  NoteId a
               | NoteOff NoteId

type Evt a = (Double, NoteEvt a) -- Timestamp in seconds, and the note event


-- Turn an Event into a NoteOn and a matching NoteOff with the same NodeId.  
eventToEvtPair :: InstrMap a -> Event -> Int -> [Evt a]
eventToEvtPair imap (Event {eTime, eInst, ePitch, eDur, eVol, eParams}) nid =
    let instr = lookupInstr eInst imap
        tOn   = fromRational eTime
        tDur  = fromRational eDur :: Double
        sf    = instr eDur ePitch eVol eParams
    in [(tOn, NoteOn nid sf), (tOn + tDur, NoteOff nid)]

-- Turn a Performance into an SF of NoteOn/NoteOffs.  
-- For each note, generate a unique id to tag the NoteOn and NoteOffs.
-- The tag is used as the key to the collection of signal functions
-- for efficient insertion/removal.
toEvtSF :: Clock p => Performance -> InstrMap a -> Signal p () [Evt a]
toEvtSF pf imap = 
    let evts = sortBy (comparing fst) $ concat $ 
                 zipWith (eventToEvtPair imap) pf [0..]
          -- Sort all NoteOn/NoteOff events by timestamp.
    in proc _ -> do
         rec
           t <- integral -< 1
           es <- init evts -< next
           let (evs, next) = span ((<= t) . fst) es
             -- Trim events that are due off the list and output them,
             -- retaining the rest
         outA -< evs

-- Modify the collection upon receiving NoteEvts.  The timestamps 
-- are not used here, but they are expected to be the same.

modSF :: M.IntMap a -> [Evt a] -> M.IntMap a
modSF = foldl' mod
    where mod m (_, NoteOn nid sf)  = M.insert nid sf m
          mod m (_, NoteOff nid)    = M.delete nid m


-- Simplified version of a parallel switcher.  
-- Note that this is tied to the particular implementation of SF, as it
-- needs to use runSF to run all the signal functions in the collection.

pSwitch :: forall p col a. (Clock p, Functor col) =>
           col (Signal p () a)  -- Initial SF collection.
        -> Signal p () [Evt (Signal p () a)]    -- Input event stream.
        -> (col (Signal p () a) -> [Evt (Signal p () a)] -> col (Signal p () a))
           -- A Modifying function that modifies the collection of SF
           --   based on the event that is occuring.
        -> Signal p () (col a)  
           -- The resulting collection of output values obtained from
           --   running all SFs in the collection.

pSwitch col esig mod = 
    proc _ -> do
      evts <- esig -< ()
      rec
        -- perhaps this can be run at a lower rate using upsample
        sfcol <- init col -< mod sfcol' evts  
        let rs = fmap (\s -> runSF (strip s) ()) sfcol :: col (a, SF () a)
            (as, sfcol' :: col (Signal p () a)) = (fmap fst rs, fmap (ArrowP . snd) rs)
      outA -< as


renderSF :: (Clock p, Performable a, AudioSample b) => 
            Music a 
         -> InstrMap (Signal p () b) 
         -> (Double, Signal p () b)
            -- ^ Duration of the music in seconds, 
            -- and a signal function that plays the music.

renderSF m im = 
    let (pf, d) = perfDur defPMap defCon m
        evtsf = toEvtSF pf im
        allsf = pSwitch M.empty evtsf modSF
        sf = allsf >>> arr (foldl' mix zero . M.elems)  -- add up all samples
    in (fromRational d, sf)
