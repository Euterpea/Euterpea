
lhs2tex --newcode MoreMusic.lhs        > ../Euterpea/Music/Note/MoreMusic.hs
lhs2tex --newcode Music.lhs            > ../Euterpea/Music/Note/Music.hs
lhs2tex --newcode Performance.lhs      > ../Euterpea/Music/Note/Performance.hs
lhs2tex --newcode SpectrumAnalysis.lhs > ../Euterpea/Music/Signal/SpectrumAnalysis.hs
REM Donya Quick wrote a FromMidi.lhs file which supercedes this one
REM lhs2tex --code FromMidi.lhs     > ../Euterpea/IO/MIDI/FromMidi.hs
REM ToDO: Convert FromMidi.lhs into a LaTeX file (perhaps as an appendix 
REM to HSoM), and return to auto-generating FromMidi.hs
lhs2tex --newcode GeneralMidi.lhs      > ../Euterpea/IO/MIDI/GeneralMidi.hs
lhs2tex --newcode ToMidi.lhs           > ../Euterpea/IO/MIDI/ToMidi.hs

lhs2tex --newcode Interlude.lhs        > ../Euterpea/Examples/Interlude.hs
lhs2tex --newcode LSystems.lhs         > ../Euterpea/Examples/LSystems.hs
lhs2tex --newcode MUI.lhs              > ../Euterpea/Examples/MUI.hs
lhs2tex --newcode RandomMusic.lhs      > ../Euterpea/Examples/RandomMusic.hs
lhs2tex --newcode SelfSimilar.lhs      > ../Euterpea/Examples/SelfSimilar.hs
lhs2tex --newcode Additive.lhs         > ../Euterpea/Examples/Additive.hs
lhs2tex --newcode SigFuns.lhs          > ../Euterpea/Examples/SigFuns.hs
