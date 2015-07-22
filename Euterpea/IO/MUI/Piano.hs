{-# LANGUAGE Arrows, CPP #-}
module Euterpea.IO.MUI.Piano where
import FRP.UISF hiding ((~++))
import FRP.UISF.UITypes
import Euterpea.Music.Note.Music hiding (transpose)
import Euterpea.IO.MUI.InstrumentBase
import Euterpea.IO.MUI.MidiWidgets ((~++))
import qualified Codec.Midi as Midi
import Data.Maybe
import qualified Data.Char as Char

#if MIN_VERSION_UISF(0,4,0)
import FRP.UISF.Graphics
import FRP.UISF.Keys
import FRP.UISF.Widget
withColorC = withColor
#else
import FRP.UISF.SOE
import qualified FRP.UISF.Widget as W
import FRP.UISF.Widget hiding (pushed, popped, marked)
pushed = let [(to,bo),(ti,bi)] = W.pushed
         in (to,ti,bi,bo)
popped = let [(to,bo),(ti,bi)] = W.popped
         in (to,ti,bi,bo)
rectangleFilled = block
withColorC = withColor'
#endif


--Note, only valid for standard US keyboards:
--Also, this is an ugly hack that can't stay
--it's mostly to test the new key events
toUpper :: Char -> Char
toUpper c = fromMaybe (Char.toUpper c) (lookup c keyMap)
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

insideKey :: KeyType -> Point -> Rect -> Bool
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
#if MIN_VERSION_UISF(0,4,0)
drawKey :: KeyType -> (Color,Color,Color,Color) -> Rect -> Graphic
#endif
drawKey White1 (to,ti,bi,bo) ((x, y), (w, h)) = 
  let val = x + w - bw `div` 2
  in withColorC ti (line (x + 1, y + 1) (x + 1, y + h - 2) //
        line (x + 1, y + 1) (val - 3, y + 1) //
        line (val - 3, y + 1 + bh) (x + w - 3, y + 1 + bh))
  // withColorC bi (line (x + 2, y + h - 2) (x + w - 2, y + h - 2) //
        line (val - 3, y + 1) (val - 3, y + 1 + bh) //
        line (x + w - 2, y + 1 + bh) (x + w - 2, y + h - 2))
  // withColorC to (line (x, y) (x, y + h - 1) //
        line (x, y) (val - 2, y) //
        line (val - 2, y + bh) (x + w - 2, y + bh))
  // withColorC bo (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
        line (val - 2, y) (val - 2, y + bh) //
        line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

drawKey White2 (to,ti,bi,bo) ((x, y), (w, h)) = 
  let valP = x + bw `div` 2
      valM = x + w - bw `div` 2
  in withColorC ti (line (valP + 3, y + 1) (valP + 3, y + bh) //
        line (valP + 3, y + 1) (valM - 3, y + 1) //
        line (x - 1, y + bh + 1) (valP - 1, y + bh + 1) //
        line (valM - 3, y + bh + 1) (x + w - 3, y + bh + 1))
  // withColorC bi (line (x + 2, y + h - 2) (x + w - 2, y + h - 2) //
        line (valM - 2, y + 1) (valM - 2, y + bh + 1) //
        line (x + w - 2, y + bh + 1) (x + w - 2, y + h - 2))
  // withColorC to (line (valP + 2, y) (valP + 2, y + bh - 1) //
        line (valP + 2, y) (valM - 2, y) //
        line (x - 2, y + bh) (valP - 2, y + bh) //
        line (valM - 2, y + bh) (x + w - 2, y + bh))
  // withColorC bo (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
        line (valM - 1, y) (valM - 1, y + bh) //
        line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

drawKey White3 (to,ti,bi,bo) ((x, y), (w, h)) = 
  let val = x + bw `div` 2
  in withColorC ti (line (val + 3, y + 1) (val + 3, y + bh) //
        line (val + 3, y + 1) (x + w - 3, y + 1) //
        line (x - 1, y + bh + 1) (val - 1, y + bh + 1))
  // withColorC bi (line (x + 2, y + h - 2) (x + w - 2, y + h - 2) //
        line (x + w - 2, y + 1) (x + w - 2, y + bh + 1) //
        line (x + w - 2, y + bh + 1) (x + w - 2, y + h - 2))
  // withColorC to (line (val + 2, y) (val + 2, y + bh - 1) //
        line (val + 2, y) (x + w - 2, y) //
        line (x - 2, y + bh) (val - 2, y + bh))
  // withColorC bo (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
        line (x + w - 1, y) (x + w - 1, y + bh) //
        line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

drawKey Black1 (to,ti,bi,bo) ((x, y), (w, h)) = 
     withColorC ti (line (x + 1, y + 1) (x + 1, y + h - 2) //
        line (x + 1, y + 1) (x + w - 3, y + 1))
  // withColorC bi (line (x + 2, y + h - 2) (x + w - 2, y + h - 2) //
        line (x + w - 2, y + 1) (x + w - 2, y + h - 2))
  // withColorC to (line (x, y) (x, y + h - 1) //
        line (x, y) (x + w - 2, y))
  // withColorC bo (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
        line (x + w - 1, y) (x + w - 1, y + h - 1))


colorKey :: KeyType -> Rect -> Graphic
colorKey Black1 r = withColor Black $ rectangleFilled r
colorKey White1 ((x,y), (w,h)) = withColor White $ 
     rectangleFilled ((x, y+bh), (ww, wh-bh))
  // rectangleFilled ((x,y), (ww - bw `div` 2, bh))
colorKey White2 ((x,y), (w,h)) = withColor White $ 
     rectangleFilled ((x, y+bh), (ww, wh-bh))
  // rectangleFilled ((x+ bw `div` 2, y), (ww-bw, bh))
colorKey White3 ((x,y), (w,h)) = withColor White $ 
     rectangleFilled ((x, y+bh), (ww, wh-bh))
  // rectangleFilled ((x+ bw `div` 2, y), (ww-bw `div` 2, bh))

-- *****************************************************************************
--   Single-key widget: handles key/mouse input and check if the song is playing
-- *****************************************************************************
mkKey :: Char -> KeyType -> UISF KeyData KeyState
mkKey c kt = mkWidget iState d process draw where
    iState = (KeyState False False False 127, Nothing)

    d = makeLayout (Fixed minw) (Fixed minh)
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
            // withColor White (drawKey kt (if isDown then pushed else popped) b) 
            // colorKey kt b
    realBBX ((x,y),(w,h)) = let (w', h') | isBlack kt = (bw,bh)
                                         | otherwise  = (ww,wh)
                             in ((x,y),(w',h'))

    process kd (kb,_) bbx evt = (kb'', (kb'', notation kd), kb /= kb'') where
        kb'  = if isJust (pressed kd) then kb { song = fromJust $ pressed kd } else kb
        kb'' = case evt of
            Key c' ms down ->
                if detectKey c' (hasShiftModifier ms)
                then kb' { keypad = down, vel = 127 }
                else kb'
#if MIN_VERSION_UISF(0,4,0)
            Button pt LeftButton down -> case (mouse kb', down, insideKey kt pt bbx) of 
#else
            Button pt True down -> case (mouse kb', down, insideKey kt pt bbx) of 
#endif
                (False, True, True) -> kb' { mouse = True,  vel = getVel pt bbx }
                (True, False, True) -> kb' { mouse = False, vel = getVel pt bbx }
                otherwise -> kb'
            MouseMove pt -> if insideKey kt pt bbx then kb' else kb' { mouse = False }
            _ -> kb'
            where getVel (u,v) ((x,y),(w,h)) = 40 + 87 * round (fromIntegral (v - y) / fromIntegral h)
                  detectKey c' s = toUpper c == toUpper c' && isUpper c == s -- This line should be more robust

-- *****************************************************************************
--   Group all keys together
-- *****************************************************************************

mkKeys :: [(Char, KeyType, AbsPitch)] -> UISF InstrumentData (SEvent [(AbsPitch, Bool, Midi.Velocity)])
mkKeys [] = constA Nothing
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
defaultMap1 = ("q2w3er5t6y7uQ@W#ER%T^Y&U", (C,2))
defaultMap2 = ("zsxdcvgbhnjmZSXDCVGBHNJM", (C,3))
defaultMap0 = (fst defaultMap1 ++ fst defaultMap2, (C,3))

piano :: PianoKeyMap -> Midi.Channel -> UISF (InstrumentData,EMM) EMM
piano (s,p) chn = focusable $ proc (instr,emm) -> do
    let emm' = fmap (setChannel chn) emm
    let instrData = instr { keyPairs = fmap mmToPair emm' }
    keys <- leftRight $ mkKeys (zip3 s defaultKeyLayout (iterate (1+) (absPitch p))) -< instrData
    returnA -< fmap (pairToMsg chn) keys ~++ emm'
