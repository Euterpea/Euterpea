{-# LANGUAGE Arrows #-}
module Euterpea.IO.MUI.Piano where
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

data KeyType = White1 | White2 | White3 | Black1 deriving (Show, Eq)

defaultKeyLayout :: [KeyType]
defaultKeyLayout = cycle [White1, Black1, White2, Black1, White3, White1, Black1, White2, Black1, White2, Black1, White3]

-- Width Height of White and Black notes
ww, wh, bw, bh, tw, th :: Int
(ww, wh) = (35, 100)
(bw, bh) = (25, 60)
(tw, th) = (8, 16)

topW :: KeyType -> Int
topW Black1 = bw `div` 2
topW White1 = ww - bw `div` 2
topW White2 = ww - bw `div` 2
topW White3 = ww

insideKey :: KeyType -> (Int,Int) -> ((Int,Int),(Int,Int)) -> Bool
insideKey Black1 pt ((x, y), (w, h)) = pt `inside` ((x,y),(bw,bh))
insideKey White1 pt ((x, y), (w, h)) =
    let b1 = ((x,y), (ww - bw `div` 2,  bh))
        b2 = ((x,  y+bh), (ww, wh-bh))
     in (pt `inside` b1) || (pt `inside` b2)
insideKey White2 pt ((x, y), (w, h)) =
    let b1 = ((x+bw `div` 2,y), (ww - bw,  bh))
        b2 = ((x,  y+bh), (ww, wh-bh))
     in (pt `inside` b1) || (pt `inside` b2)
insideKey White3 pt ((x, y), (w, h)) =
    let b1 = ((x+bw `div` 2,y), (bw `div` 2,  bh))
        b2 = ((x,  y+bh), (ww, wh-bh))
     in (pt `inside` b1) || (pt `inside` b2)

isBlack :: KeyType -> Bool
isBlack Black1 = True
isBlack _      = False


-- *****************************************************************************
--   Drawing routines for each key type
-- *****************************************************************************
-- This has a complicated type, so I'm leaving it out.
drawBox kt | kt == White1 = white1
           | kt == White2 = white2
           | kt == White3 = white3
           | kt == Black1 = black1

white1 [] _ = nullGraphic
white1 ((t, b):cs) ((x, y), (w, h)) =
    let x' = x + w - bw `div` 2
        y' = y + bh
     in white1 cs ((x + 1, y + 1), (w - 2, h - 2)) //
        withColor' t (line (x, y) (x, y + h - 1) //
            line (x, y) (x' - 2, y) //
            line (x' - 2, y+bh) (x + w - 2, y+bh)) //
        withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
            line (x + w - 2 - bw `div` 2, y) (x + w - 2 - bw `div` 2, y+bh) //
            line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

white2 [] _ = nullGraphic
white2 ((t, b):cs) ((x, y), (w, h)) = 
    let x1 = x + bw `div` 2
        x2 = x + w - bw `div` 2
        y' = y + bh
     in white2 cs ((x + 1, y + 1), (w - 2, h - 2)) //
        withColor' t (line (x1+2, y) (x1+2, y' - 1) //
            line (x1+2, y) (x2 - 2, y) //
            line (x - 2, y') (x1 - 2, y') //
            line (x2- 2, y') (x + w - 2, y')) //
        withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
            line (x2 - 1, y) (x2 - 1, y') //
            line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

white3 [] _ = nullGraphic
white3 ((t, b):cs) ((x, y), (w, h)) =
    let x1 = x + bw `div` 2
        y' = y + bh
     in white3 cs ((x + 1, y + 1), (w - 2, h - 2)) //
        withColor' t (line (x1+2, y) (x1+2, y' - 1) //
            line (x1+2, y) (x + w - 2, y) //
            line (x - 2, y') (x1 - 2, y')) //
        withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
            line (x + w - 1, y) (x + w - 1, y') //
            line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

black1 [] _ = nullGraphic
black1 ((t, b):cs) ((x, y), (w, h)) =
    black1 cs ((x + 1, y + 1), (w - 2, h - 2)) //
    withColor' t (line (x, y) (x, y + h - 1) //
        line (x, y) (x + w - 2, y)) //
    withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
        line (x + w - 1, y) (x + w - 1, y + h - 1))

colorKey Black1 b = withColor Black $ block b
colorKey kt ((x,y), (w,h)) = withColor White $ block ((x, y+bh), (ww, wh-bh)) // f kt
    where f White1 = block ((x,y), (ww - bw `div` 2, bh))
          f White2 = block ((x+ bw `div` 2, y), (ww-bw, bh))
          f White3 = block ((x+ bw `div` 2, y), (ww-bw `div` 2, bh))

-- *****************************************************************************
--   Single-key widget: handles key/mouse input and check if the song is playing
-- *****************************************************************************
mkKey :: Char -> KeyType -> UISF KeyData KeyState
mkKey c kt = mkWidget iState d process draw where
    iState = (KeyState False False False 127, Nothing)

    d = Layout 0 0 0 minh minw minh
    minw = topW kt
    minh | isBlack kt = bh
         | otherwise  = wh

    draw rect inFocus (kb, showNote) = 
        let isDown = isKeyDown kb
            b@((x,y),(w,h)) = realBBX rect
            x' = x + (w - tw) `div` 2 + if isDown then 0 else -1
            y' = y + h `div` 3 + (h - th) `div` 2 + if isDown then 0 else -1
            drawNotation s = withColor Red $ text (x'+(1-length s)*tw `div` 2, y'- th + 2) s
         in withColor (if isBlack kt then White else Black) (text (x',y') [c]) 
            // maybe nullGraphic drawNotation showNote 
            // withColor White (drawBox kt (if isDown then pushed else popped) b) 
            // colorKey kt b
    realBBX ((x,y),(w,h)) = let (w', h') | isBlack kt = (bw,bh)
                                         | otherwise  = (ww,wh)
                             in ((x,y),(w',h'))

    process kd (kb,_) bbx evt = (kb'', (kb'', notation kd), kb /= kb'') where
        kb'  = if isJust (pressed kd) then kb { song = fromJust $ pressed kd } else kb
        kb'' = case evt of
            Key (CharKey c') down ms ->
                if detectKey c' (shift ms)
                then kb' { keypad = down, vel = 127 }
                else kb'
            Button pt True down -> case (mouse kb', down, insideKey kt pt bbx) of 
                (False, True, True) -> kb' { mouse = True,  vel = getVel pt bbx }
                (True, False, True) -> kb' { mouse = False, vel = getVel pt bbx }
                otherwise -> kb'
            MouseMove pt -> if insideKey kt pt bbx then kb' else kb' { mouse = False }
            otherwise -> kb'
            where getVel (u,v) ((x,y),(w,h)) = 40 + 87 * round ((fromIntegral v - fromIntegral y) / fromIntegral h)
                  detectKey c' s = toUpper c == toUpper c' && isUpper c == s -- This line should be more robust

-- *****************************************************************************
--   Group all keys together
-- *****************************************************************************

mkKeys :: [(Char, KeyType, AbsPitch)] -> UISF InstrumentData (SEvent [(AbsPitch, Bool, Midi.Velocity)])
mkKeys [] = proc instr -> returnA -< Nothing
mkKeys ((c,kt,ap):ckas) = proc instr -> do
    msg <- unique <<< mkKey c kt -< getKeyData ap instr
    let on  = maybe False isKeyPlay msg
        ped = pedal instr
        ret | not on && not ped = [(ap, False, maybe 0   vel msg)]
            | on                = [(ap, True,  maybe 127 vel msg)]
            | otherwise         = []
    msgs <- mkKeys ckas -< instr
    returnA -< fmap (const ret) msg ~++ msgs

-- *****************************************************************************
--   Main widget: piano that takes a map (string) of characters to map to notes
--   and the pitch of the first note
--   two default maps are provided so that two piano can be loaded concurrently
-- *****************************************************************************
type PianoKeyMap = (String, Pitch)
defaultMap1, defaultMap2, defaultMap0 :: PianoKeyMap
defaultMap1 = ("q2w3er5t6y7uQ@W#ERT^Y&U*", (C,2))
defaultMap2 = ("zsxdcvgbhnjmZSXDCVGBHNJM", (C,3))
defaultMap0 = (fst defaultMap1 ++ fst defaultMap2, (C,3))

piano :: PianoKeyMap -> Midi.Channel -> UISF (InstrumentData,EMM) EMM
piano (s,p) chn = focusable $ proc (instr,emm) -> do
    let emm' = fmap (setChannel chn) emm
    let instrData = instr { keyPairs = fmap mmToPair emm' }
    keys <- leftRight $ mkKeys (zip3 s defaultKeyLayout (iterate (1+) (absPitch p))) -< instrData
    returnA -< fmap (pairToMsg chn) keys ~++ emm'
