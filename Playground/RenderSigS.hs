{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}
module RenderSigS where
import Euterpea hiding (Table, tableSinesN, osc, oscFixed, exportFile, NoteOn, NoteOff, transpose)
import Data.Maybe
import Data.List
import Data.Ord
import SigS

data NoteEvt a = NoteDur Double a
type Evt a = (Double, NoteEvt a)

renderSigS :: (Performable a, AudioSample b, Ord b, Fractional b) => Music a -> InstrMap (SigS b) -> (Double, SigS b)
renderSigS m im = 
    let (pf, d) = perfDur defPMap defCon m
        evts  = toEvts pf im
        sigs  = map (\(t, NoteDur dur sig) -> (sDelay t (sSample dur sig))) evts
        total = map (foldl' mix zero) (transpose (map runSigS sigs))
        maxS  = foldl1' max total
     in (fromRational d, SigS (map (/maxS) total))

toEvts :: Performance -> InstrMap a -> [Evt a]
toEvts pf imap = sortBy (comparing fst) $ concatMap (eventToEvtPair imap) pf

eventToEvtPair :: InstrMap a -> Event -> [Evt a]
eventToEvtPair imap (Event {eTime, eInst, ePitch, eDur, eVol, eParams}) =
    let instr = lookupInstr eInst imap
        tOn   = fromRational eTime
        tDur  = fromRational eDur :: Double
        sig   = instr eDur ePitch eVol eParams
     in [(tOn, NoteDur tDur sig)]

lookupInstr :: InstrumentName -> InstrMap a -> Instr a
lookupInstr ins im = fromMaybe (error $ "Instrument " ++ show ins ++ " does not have a matching Instr in the supplied InstrMap.") (lookup ins im)
