{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}
module RenderSigS where
import Euterpea hiding (Table, tableSinesN, osc, oscFixed, exportFile, NoteOn, NoteOff, transpose)
import Euterpea.Music.Signal.SigFuns (AudSF)
import Data.Array.Unboxed hiding (accum)
import qualified Data.IntMap as M
import System.CPUTime
import Control.Arrow
import Text.Printf
import Data.Audio
import Codec.Wav
import Data.List
import Data.Int
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
        final = map (/ maximum total) total
     in (fromRational d, SigS final)

toEvts :: Performance -> InstrMap a -> [Evt a]
toEvts pf imap = sortBy (comparing fst) $ concat $ map (eventToEvtPair imap) pf

eventToEvtPair :: InstrMap a -> Event -> [Evt a]
eventToEvtPair imap (Event {eTime, eInst, ePitch, eDur, eVol, eParams}) =
    let instr = lookupInstr eInst imap
        tOn   = fromRational eTime
        tDur  = fromRational eDur :: Double
        sig   = instr eDur ePitch eVol eParams
     in [(tOn, NoteDur tDur sig)]

lookupInstr :: InstrumentName -> InstrMap a -> Instr a
lookupInstr ins im =
    case lookup ins im of
      Just i -> i
      Nothing -> error $ "Instrument " ++ show ins ++ 
                 " does not have a matching Instr in the supplied InstrMap."