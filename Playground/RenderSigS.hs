{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}
module RenderSigS where
import Euterpea hiding (Table, tableSinesN, osc, oscFixed, exportFile, NoteOn, NoteOff, transpose)
import Data.Maybe
import Data.List
import Data.Ord
import SigS

data Evt a = Evt { tOn :: Double, tDur :: Double, sig :: a } -- Time, Duration, a

-- This function converts a musical value into a Duration, SigS pair.
renderSigS :: (Performable a, AudioSample b, Ord b, Fractional b) => Music a -> InstrMap (SigS b) -> (Double, SigS b)
renderSigS m im = 
    let (pf, d) = perfDur defPMap defCon m
        evts  = toEvts pf im
        -- Converts each time/duration pair into its own signal, delayed by its offset and sampled for its duration
        sigs  = map (\(Evt t dur sig) -> (sDelay t (sSample dur sig))) evts
        -- total and maxS are strict to avoid memory leaks
        -- Each signal is run and the corresponding samples are mixed.
        total = map (foldl' mix zero) (transpose (map runSigS sigs))
        maxS  = foldl1' max total
        -- Normalize and return the signal.
     in (fromRational d, SigS (map (/maxS) total))

-- Turns a Performance into a list of simple events indicating time, duration and pitch.
-- Returns them in time-sorted order.
toEvts :: Performance -> InstrMap a -> [Evt a]
toEvts pf imap = sortBy (comparing tOn) $ map (eventToEvt imap) pf

-- Converts a Performance Event into a single event containing only the relevant information,
-- plus the signal used to generate the sound for the original event.
eventToEvt :: InstrMap a -> Event -> Evt a
eventToEvt imap (Event {eTime, eInst, ePitch, eDur, eVol, eParams}) =
    let instr = lookupInstr eInst imap
        tOn   = fromRational eTime
        tDur  = fromRational eDur :: Double
        sig   = instr eDur ePitch eVol eParams
     in Evt tOn tDur sig

-- Attempts to find the given instrument in the map, dying if it cannot.
lookupInstr :: InstrumentName -> InstrMap a -> Instr a
lookupInstr ins im = fromMaybe (error $ "Instrument " ++ show ins ++ " does not have a matching Instr in the supplied InstrMap.") (lookup ins im)
