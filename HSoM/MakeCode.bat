
lhs2tex --code MoreMusic.lhs        > ../Euterpea/Music/Note/MoreMusic.hs
lhs2tex --code Music.lhs            > ../Euterpea/Music/Note/Music.hs
lhs2tex --code Performance.lhs      > ../Euterpea/Music/Note/Performance.hs
lhs2tex --code SpectrumAnalysis.lhs > ../Euterpea/Music/Signal/SpectrumAnalysis.hs
REM Donya Quick wrote a FromMidi.lhs file which supercedes this one
REM lhs2tex --code FromMidi.lhs     > ../Euterpea/IO/MIDI/FromMidi.hs
REM ToDO: Convert FromMidi.lhs into a LaTeX file (perhaps as an appendix 
REM to HSoM), and return to auto-generating FromMidi.hs
lhs2tex --code GeneralMidi.lhs      > ../Euterpea/IO/MIDI/GeneralMidi.hs
lhs2tex --code ToMidi.lhs           > ../Euterpea/IO/MIDI/ToMidi.hs

lhs2tex --code Interlude.lhs        > ../Euterpea/Examples/Interlude.hs
lhs2tex --code LSystems.lhs         > ../Euterpea/Examples/LSystems.hs
lhs2tex --code MUI.lhs              > ../Euterpea/Examples/MUI.hs
lhs2tex --code RandomMusic.lhs      > ../Euterpea/Examples/RandomMusic.hs
lhs2tex --code SelfSimilar.lhs      > ../Euterpea/Examples/SelfSimilar.hs
lhs2tex --code Additive.lhs         > ../Euterpea/Examples/Additive.hs
lhs2tex --code SigFuns.lhs          > ../Euterpea/Examples/SigFuns.hs
