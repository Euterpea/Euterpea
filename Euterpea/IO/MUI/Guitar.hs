{-# LANGUAGE Arrows #-}
module Euterpea.IO.MUI.Guitar where
import Euterpea.IO.MUI.UIMonad
import Euterpea.IO.MUI.Widget
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.SOE
import Euterpea.IO.MIDI
import Euterpea.Music.Note.Music hiding (transpose)
import Euterpea.Music.Note.Performance
import Control.SF.AuxFunctions
import Control.Arrow
import Euterpea.IO.MUI.InstrumentBase
import qualified Codec.Midi as Midi
import Data.Maybe
import qualified Data.Char as Char

--Note, only valid for standard US keyboards:
--Also, this is an ugly hack that can't stay
--it's mostly to test the new key events
toUpper :: Char -> Char
toUpper c = case lookup c keyMap of
                Just c' -> c'
                Nothing -> Char.toUpper c
            where keyMap = [('`', '~'), ('1', '!'), ('2', '@'), ('3', '#'), ('4', '$'),
                            ('5', '%'), ('6', '^'), ('7', '&'), ('8', '*'), ('9', '('),
                            ('0', ')'), ('-', '_'), ('=', '+'), ('[', '{'), (']', '}'),
                            ('|', '\\'), ('\'', '\"'), (';', ':'), ('/', '?'), ('.', '>'),
                            (',', '<')]

isUpper :: Char -> Bool
isUpper c = toUpper c == c

-- first fret's width and height
fw,fh,tw,th :: Int
(fw, fh) = (90, 45)
(tw, th) = (8, 16)

type KeyType = Int
type GuitarKeyMap = [(String, Pitch, Char)]

-- Draws an individual fret

drawFret [] ((x, y), (w, h)) = nullGraphic
drawFret ((t, b):cs) ((x, y), (w, h)) =
    drawFret cs ((x + 1, y + 1), (w - 2, h )) //
    withColor' t (line (x, y) (x, y + h)) //
    withColor' b (line (x + w - 1, y) (x + w - 1, y + h))

-- Draws the string on top of each fret
    
drawString down ((x, y), (w, h)) =
    withColor Black (if down then arc (x,midY+2) (x+w, midY-2) (-180) 180
                             else line (x-1, y+ h `div` 2) (x+w, y+h `div` 2)) //
    if down then withColor Blue (ellipse (midX - d, midY - d) (midX + d, midY + d)) else nullGraphic
    where d = 10
          midX = x + w `div` 2
          midY = y + h `div` 2

-- Draws just the guitar head, not interactive

drawHead :: Int -> UISF () ()
drawHead n = topDown $ constA (repeat ()) >>>
             concatA (map (\k -> mkBasicWidget layout (draw k)) [n,n-1..1]) >>>
             constA ()
    where draw k ((x,y),(w,h)) = withColor Black $ line (x, y + h `div` 2 + 5 * (3 - k)) (x + w, y + h `div` 2)
          layout = Layout 0 0 fw fh fw fh


--drawHead :: Int -> UISF () ()
--drawHead 0 = proc _ -> returnA -< ()
--drawHead n = topDown $  proc _ -> do
--    ui <- mkBasicWidget layout draw -< ()
--    ui' <- drawHead (n-1) -< ()
--    returnA -< ()
--    where draw ((x,y),(w,h)) = withColor Black $ line (x, y + h `div` 2 + 5 * (3 - n)) (x + w, y + h `div` 2)
--          layout = Layout 0 0 fw fh fw fh

-- Given a character to respond to, and which fret it is, draws and displays a single interactive fret
          
mkKey :: Char -> KeyType -> UISF KeyData KeyState
mkKey c kt = mkWidget iState d process draw where
    iState = (KeyState False False False 127, Nothing)

    d = Layout 0 0 0 minh minw minh
    (minh, minw) = (fh, fw - kt * 3)

    draw box@((x,y),(w,h)) _ (kb, showNote) =
        let isDown = isKeyDown kb
            x' = x + (w - tw) `div` 2 + if isDown then 0 else -1
            y' = y + h `div` 5 + (h - th) `div` 2 + if isDown then 0 else -1
            drawNotation s = withColor Red $ text (x' + (1 - length s) * tw `div` 2, y' - th) s
         in withColor Blue (text (x', y') [c]) 
            // maybe nullGraphic drawNotation showNote 
            // drawString isDown box 
            // drawFret popped box

    process kd (kb,_) box evt = (kb'', (kb'', notation kd), kb /= kb'') where
        kb' = if isJust (pressed kd) then kb { song = fromJust $ pressed kd } else kb
        kb'' = case evt of
            Key (CharKey c') down ms ->
                if detectKey c' (shift ms)
                then kb' { keypad = down, vel = 127 }
                else kb'
            Button pt True down ->
                case (mouse kb', down, pt `inside` box) of
                    (False, True, True) -> kb' { mouse = True,  vel = getVel pt box }
                    (True, False, True) -> kb' { mouse = False, vel = getVel pt box }
                    otherwise -> kb'
            MouseMove pt ->
                if pt `inside` box
                then kb'
                else kb' { mouse = False }
            otherwise -> kb'
            where getVel (u,v) ((x,y),(w,h)) = 127 - round (87 * (abs (fromIntegral u - fromIntegral (2 * x + w) / 2) / (fromIntegral w / 2)))
                  detectKey c' s = toUpper c == toUpper c' && isUpper c == s -- This line should be more robust

-- Makes all of the frets on a string, returning the combined list of their outputs
        
mkKeys :: AbsPitch -> [(Char, KeyType, AbsPitch)] -> UISF (Bool, InstrumentData) (SEvent [(AbsPitch, Bool, Midi.Velocity)])
mkKeys _ [] = proc _ -> returnA -< Nothing
mkKeys free ((c,kt,ap):ckas) = proc (pluck, instr) -> do
    msg <- unique <<< mkKey c kt -< getKeyData ap instr
    let on  = maybe False isKeyPlay msg
        ret | pluck     = if on then [(ap, True, maybe 127 vel msg)] else [(free, True, 127)]
            | not pluck = [(ap, False, maybe 0 vel msg)]
    msgs <- mkKeys free ckas -< (pluck, instr)
    returnA -< fmap (const ret) msg ~++ msgs

-- Creates the whole string, including the response to the strum key
    
mkString :: ([Char], Pitch, Char) -> UISF InstrumentData (SEvent [(AbsPitch, Bool, Midi.Velocity)])
mkString (frets, freePitch, p) = leftRight $ proc insData -> do
    isPluck <- pluckString p -< ()
    msgs <- mkKeys freeap (zip3 frets [1..] [freeap+1..]) -< (isPluck, insData)
    returnA -< msgs
    where freeap = absPitch freePitch

-- Invisible widget that responds to a single character
-- There should really be built-in behavior for this sort of thing
    
pluckString :: Char -> UISF () Bool
pluckString c = mkWidget False nullLayout process draw where
    draw ((x,y),(w,h)) _ down =
        let x' = x + (w - tw) `div` 2 + if down then 0 else -1
            y' = y + (h - th) `div` 2 + if down then 0 else -1
         in withColor (if down then White else Black) $ block ((0,0),(10,10))

    process _ s _ evt = (s', s', s /= s') where
        s' = case evt of
            Button pt True down -> down
            Key (CharKey c') down _ ->
                down && c == c'
            otherwise -> s

-- Assembles the whole guitar according to a given key map and channel
-- Requires a persistent instrument data object to be passed in.
-- Any midi messages passed to the guitar will be played on all applicable frets
-- Outputs its midi messages as generated by its inputs and user interaction
            
guitar :: GuitarKeyMap -> Midi.Channel -> UISF (InstrumentData,EMM) EMM
guitar spcList chn = focusable $ leftRight $ proc (instr, emm) -> do
    let emm' = fmap (setChannel chn) emm
    h <- drawHead (length spcList) -< ()
    frets <- mkStrings spcList -< instr { keyPairs = fmap mmToPair emm' }
    returnA -< fmap (pairToMsg chn) frets ~++ emm'
    where mkStrings [] = proc _ -> returnA -< Nothing
          mkStrings (spc:spcs) = topDown $ proc instrData -> do
              msg <- mkString spc -< instrData
              msgs <- mkStrings spcs -< instrData
              returnA -< msg ~++ msgs

-- The default six string keymap. The first in the tuple determines how many frets
-- will be displayed and what their activator keys are. The second in the tuple
-- is the open pitch (that is, the note that is played when no frets are pressed)
-- and the final entry is the strum key.
              
string1, string2, string3, string4, string5, string6 :: (String, Pitch, Char)
string6 = ("1qaz__________", (E,5), '\b')
string5 = ("2wsx__________", (B,4), '=')
string4 = ("3edc__________", (G,4), '-')
string3 = ("4rfv__________", (D,4), '0')
string2 = ("5tgb__________", (A,3), '9')
string1 = ("6yhn__________", (E,3), '8')

sixString = reverse [string1, string2, string3, string4, string5, string6]
