module Euterpea.IO.MUI.InstrumentWidgets (
    PianoKeyMap, GuitarKeyMap,
    KeyData, KeyState, InstrumentData,
    defaultInstrumentData, defaultKeyLayout, defaultMap0, defaultMap1, defaultMap2,
    addNotation, addTranspose, addPedal, addEcho,
    selectInstrument, songPlayer,
    piano, guitar, sixString
) where
import Euterpea.IO.MUI.InstrumentBase
import Euterpea.IO.MUI.Guitar
import Euterpea.IO.MUI.Piano
