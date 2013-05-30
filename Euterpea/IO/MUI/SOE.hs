module Euterpea.IO.MUI.SOE (
  runGraphics,
  Title,
  Size,
  Window,
  openWindow,
  getMainWindowSize,
  clearWindow,
  drawInWindow,
  drawInWindowNow,
  setGraphic,
  setGraphic',
  setDirty,
  closeWindow,
  openWindowEx,
  RedrawMode,
  drawGraphic,
  drawBufferedGraphic,
  Graphic,
  nullGraphic,
  emptyGraphic,
  overGraphic ,
  overGraphics,
  translateGraphic,
  Color (..),
  RGB,
  RGBA,
  rgb,
  rgba,
  withColor,
  withColor',
  text,
  Point,
  ellipse,
  shearEllipse,
  line,
  polygon,
  polyline,
  polyBezier,
  Angle,
  arc,
  scissorGraphic,
--  Region,     --Regions are an unused feature
--  createRectangle,
--  createEllipse,
--  createPolygon,
--  andRegion,
--  orRegion,
--  xorRegion,
--  diffRegion,
--  drawRegion,
--  getKey,     -- See note at definition for why these are left out
--  getLBP,
--  getRBP,
  KeyModifiers (..),
  Key(..), 
  SpecialKey (..),
  UIEvent (..),
  maybeGetWindowEvent,
  getWindowEvent,
  Word32,
  timeGetTime,
  word32ToInt,
  isKeyPressed
  ) where

import Data.Ix (Ix)
import Data.Word (Word32)
import Graphics.UI.GLFW (Key(..), SpecialKey(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Control.Concurrent.MVar


-------------------
-- Window Functions
-------------------

runGraphics :: IO () -> IO ()
runGraphics main = main

type Title = String
type Size = (Int, Int)

data Window = Window {
  graphicVar :: MVar (Graphic, Bool), -- boolean to remember if it's dirty
  eventsChan :: TChan UIEvent
}

-- Graphic is just a wrapper for OpenGL IO
newtype Graphic = Graphic (IO ())

initialized, opened :: MVar Bool
initialized = unsafePerformIO (newMVar False)
opened = unsafePerformIO (newMVar False)

initialize = do
  init <- readMVar initialized
  if init then return ()
    else do
      GLFW.initialize
      modifyMVar_ initialized (\_ -> return True)
      return ()

openWindow :: Title -> Size -> IO Window
openWindow title size =
  openWindowEx title Nothing (Just size) drawBufferedGraphic

-- pos is always ignored due to GLFW
openWindowEx :: Title -> Maybe Point -> Maybe Size -> RedrawMode -> IO Window
openWindowEx title position size (RedrawMode useDoubleBuffer) = do
  let siz = maybe (GL.Size 400 300) fromSize size
  initialize
  graphicVar <- newMVar (emptyGraphic, False)
  eventsChan <- atomically newTChan
  GLFW.openWindow siz [GLFW.DisplayStencilBits 8, GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= title
  modifyMVar_ opened (\_ -> return True)
  GL.shadeModel $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth $= 1.5

  -- this will hang on Windows
  -- let updateWindow = readMVar graphicVar >>= (\(Graphic g) -> g >> GLFW.swapBuffers)
  -- GLFW.windowRefreshCallback $= updateWindow

  let motionCallback (GL.Position x y) = atomically $ 
        writeTChan eventsChan MouseMove { pt = (fromIntegral x, fromIntegral y) }
  GLFW.mousePosCallback $= motionCallback
     
  let charCallback char state = atomically $ writeTChan eventsChan (Character char)
  let keyCallBack  key  state = do
      lshift <- isKeyPressed (SpecialKey LSHIFT)
      lctrl  <- isKeyPressed (SpecialKey LCTRL)
      lalt   <- isKeyPressed (SpecialKey LALT)
      rshift <- isKeyPressed (SpecialKey RSHIFT)
      rctrl  <- isKeyPressed (SpecialKey RCTRL)
      ralt   <- isKeyPressed (SpecialKey RALT)
      atomically $ writeTChan eventsChan (Key { key = key, isDown = (state == GLFW.Press),
                    modifiers = KeyModifiers {shift = lshift || rshift, ctrl = lctrl || rctrl, alt = lalt || ralt}})
  
  GLFW.charCallback $= charCallback 
  GLFW.keyCallback  $= keyCallBack
  GLFW.enableSpecial GLFW.KeyRepeat

  GLFW.mouseButtonCallback $= (\but state -> do
    GL.Position x y <- GL.get GLFW.mousePos
    atomically $ writeTChan eventsChan (Button {
        pt = (fromIntegral x, fromIntegral y),
        isLeft = (but == GLFW.ButtonLeft),
        isDown = (state == GLFW.Press) }))

  GLFW.windowSizeCallback $= atomically . writeTChan eventsChan . Resize
  GLFW.windowRefreshCallback $= atomically (writeTChan eventsChan Refresh)
  GLFW.windowCloseCallback $= (closeWindow_ eventsChan >> return True)

  return Window {
    graphicVar = graphicVar,
    eventsChan = eventsChan
  }

getMainWindowSize :: IO Size
getMainWindowSize = do
  (GL.Size x y) <- GL.get GLFW.windowSize
  return (fromIntegral x, fromIntegral y)

clearWindow :: Window -> IO ()
clearWindow win = setGraphic win (Graphic (return ()))

drawInWindow :: Window -> Graphic -> IO ()
drawInWindow win graphic = 
  modifyMVar_ (graphicVar win) (\ (g, _) -> 
    return (overGraphic graphic g, True)) 

-- if window is marked as dirty, mark it clean, draw and swap buffer;
-- otherwise do nothing.
updateWindowIfDirty win = do
   io <- modifyMVar (graphicVar win) (\ (g@(Graphic io), dirty) -> do
     return ((g, False), if dirty then io >> GLFW.swapBuffers
                                  else return ()))
   io

drawInWindowNow :: Window -> Graphic -> IO ()
drawInWindowNow win graphic = do
  drawInWindow win graphic
  updateWindowIfDirty win

-- setGraphic set the given Graphic over empty (black) background for
-- display in current Window.
setGraphic :: Window -> Graphic -> IO ()
setGraphic win graphic = do
  modifyMVar_ (graphicVar win) (\_ -> 
    return (overGraphic graphic emptyGraphic, True))

setGraphic' :: Window -> Graphic -> IO ()
setGraphic' win graphic = do
  modifyMVar_ (graphicVar win) (\(_, dirty) -> 
    return (overGraphic graphic emptyGraphic, dirty))

setDirty :: Window -> IO ()
setDirty win = do
  modifyMVar_ (graphicVar win) (\(g, _) -> return (g, True))

closeWindow :: Window -> IO ()
closeWindow win = closeWindow_ (eventsChan win)

closeWindow_ chan = do
  atomically $ writeTChan chan Closed
  modifyMVar_ opened (\_ -> return False)
  GLFW.closeWindow
  GLFW.pollEvents

--------------------
-- Drawing Functions
--------------------

newtype RedrawMode = RedrawMode Bool

drawGraphic :: RedrawMode
drawGraphic = RedrawMode False

drawBufferedGraphic :: RedrawMode
drawBufferedGraphic = RedrawMode True

data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

type Angle = GLfloat

nullGraphic :: Graphic
nullGraphic = Graphic $ return ()

emptyGraphic :: Graphic
emptyGraphic = Graphic $ do 
  GL.clearColor $= GL.Color4 (0xec/0xff) (0xe9/0xff) (0xd8/0xff) (0x00) -- GL.Color4 0 0 0 0
  GL.clear [GL.ColorBuffer, GL.StencilBuffer]

translateGraphic :: (Int, Int) -> Graphic -> Graphic
translateGraphic (x, y) (Graphic g) = Graphic $ GL.preservingMatrix $ do
  GL.translate (GL.Vector3 (fromIntegral x) (fromIntegral y) (0::GLfloat))
  g

overGraphic :: Graphic -> Graphic -> Graphic
overGraphic (Graphic over) (Graphic base) = Graphic (base >> over)

overGraphics :: [Graphic] -> Graphic
overGraphics = foldl1 overGraphic

colorToRGB :: Color -> GL.Color3 GLfloat
colorToRGB Black   = GL.Color3 0 0 0
colorToRGB Blue    = GL.Color3 0 0 1
colorToRGB Green   = GL.Color3 0 1 0
colorToRGB Cyan    = GL.Color3 0 1 1
colorToRGB Red     = GL.Color3 1 0 0
colorToRGB Magenta = GL.Color3 1 0 1
colorToRGB Yellow  = GL.Color3 1 1 0
colorToRGB White   = GL.Color3 1 1 1

withColor :: Color -> Graphic -> Graphic
withColor color = withColor' (colorToRGB color)

withColor' :: GL.Color a => a -> Graphic -> Graphic
withColor' color (Graphic g) = Graphic (GL.color color >> g)

type RGB = GL.Color3 GL.GLfloat
type RGBA = GL.Color4 GL.GLfloat
rgb r g b = GL.Color3 (c2f r) (c2f g) (c2f b) :: RGB
rgba r g b a = GL.Color4 (c2f r) (c2f g) (c2f b) (c2f a) :: RGBA
c2f i = fromIntegral i / 255

text :: Point -> String -> Graphic
text (x,y) str = Graphic $ GL.preservingMatrix $ do
  GL.translate (GL.Vector3 (fromIntegral x) (fromIntegral y + 16) (0::GLfloat))
  GL.scale 1 (-1) (1::GLfloat)
  GLFW.renderString GLFW.Fixed8x16 str

type Point = (Int, Int)

ellipse :: Point -> Point -> Graphic
ellipse pt1 pt2 = Graphic $ GL.preservingMatrix $ do
  let (x, y, width, height) = normaliseBounds pt1 pt2
      (r1, r2) = (width / 2, height / 2)
  GL.translate (GL.Vector3 (x + r1) (y + r2) 0)
  GL.renderPrimitive GL.Polygon (circle r1 r2 0 (2 * pi) (6 / (r1 + r2)))
      
shearEllipse :: Point -> Point -> Point -> Graphic
shearEllipse p0 p1 p2 = Graphic $ 
  let (x0,y0) = fromPoint p0
      (x1,y1, w, h) = normaliseBounds p1 p2
      (x2,y2) = (x1 + w, y1 + h)
      x =  (x1 + x2) / 2  -- centre of parallelogram
      y =  (y1 + y2) / 2
      dx1 = (x1 - x0) / 2 -- distance to corners from centre
      dy1 = (y1 - y0) / 2
      dx2 = (x2 - x0) / 2
      dy2 = (y2 - y0) / 2
      pts = [ (x + c*dx1 + s*dx2, y + c*dy1 + s*dy2)
            | (c,s) <- cos'n'sins ]
      cos'n'sins = [ (cos a, sin a) | a <- segment 0 (2 * pi) (40 / (w + h))]
  in GL.renderPrimitive GL.Polygon $ 
        mapM_ (\ (x, y) -> GL.vertex (vertex3 x y 0)) pts

line :: Point -> Point -> Graphic
line (x1, y1) (x2, y2) = Graphic $ 
  GL.renderPrimitive GL.LineStrip (do
    GL.vertex (vertex3 (fromIntegral x1) (fromIntegral y1) 0)
    GL.vertex (vertex3 (fromIntegral x2) (fromIntegral y2) 0))

polygon :: [Point] -> Graphic
polygon ps = Graphic $ do
  GL.renderPrimitive GL.Polygon (foldr1 (>>) (map 
    (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0))
    ps))

polyline :: [Point] -> Graphic
polyline ps = Graphic $ 
  GL.renderPrimitive GL.LineStrip (foldr1 (>>) (map 
    (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0))
    ps))

polyBezier :: [Point] -> Graphic
polyBezier [] = Graphic $ return ()
polyBezier ps = polyline (map (bezier ps) (segment 0 1 dt))
  where
    dt = 1 / (lineLength ps / 8)
    lineLength :: [Point] -> GLfloat
    lineLength ((x1,y1):(x2,y2):ps) = 
      let dx = x2 - x1
          dy = y2 - y1
      in sqrt (fromIntegral (dx * dx + dy * dy)) + lineLength ((x2,y2):ps)
    lineLength _ = 0

bezier :: [Point] -> GLfloat -> Point
bezier [(x1,y1)] t = (x1, y1)
bezier [(x1,y1),(x2,y2)] t = (x1 + truncate (fromIntegral (x2 - x1) * t), 
                              y1 + truncate (fromIntegral (y2 - y1) * t))
bezier ps t = bezier (map (\ (p, q) -> bezier [p,q] t) (zip ps (tail ps))) t

arc :: Point -> Point -> Angle -> Angle -> Graphic
arc pt1 pt2 start extent = Graphic $ GL.preservingMatrix $ do
  let (x, y, width, height) = normaliseBounds pt1 pt2
      (r1, r2) = (width / 2, height / 2)
  GL.translate (GL.Vector3 (x + r1) (y + r2) 0)
  GL.renderPrimitive GL.LineStrip (circle r1 r2 
    (-(start + extent) * pi / 180) (-start * pi / 180) (6 / (r1 + r2)))


scissorGraphic :: (Point, Size) -> Graphic -> Graphic
scissorGraphic ((x,y), (w,h)) (Graphic g) = Graphic $ do
    (_,windowY) <- getMainWindowSize
    let [x', y', w', h'] = map fromIntegral [x, windowY-y-h, w, h]
    oldScissor <- GL.get GL.scissor
    GL.scissor $= Just (GL.Position x' y', GL.Size w' h')
    g
    GL.scissor $= oldScissor


-------------------
-- Region Functions
-------------------

{- Unused

createRectangle :: Point -> Point -> Region
createRectangle pt1 pt2 =
  let (x,y,width,height) = normaliseBounds' pt1 pt2 
      [x0, y0, x1, y1] = map fromIntegral [x, y, x + width, y + height]
      drawing = 
        GL.renderPrimitive GL.Quads (do
        GL.vertex (vertex3 x0 y0 0)
        GL.vertex (vertex3 x1 y0 0)
        GL.vertex (vertex3 x1 y1 0)
        GL.vertex (vertex3 x0 y1 0))
   in [[Pos ("R" ++ show (x0,y0,x1,y1), drawing)]]

createEllipse :: Point -> Point -> Region
createEllipse pt1 pt2 =
  let (x,y,width,height) = normaliseBounds' pt1 pt2
      drawing = 
        GL.preservingMatrix $ do
          let (x, y, width, height) = normaliseBounds pt1 pt2
              (r1, r2) = (width / 2, height / 2)
          GL.translate (GL.Vector3 (x + r1) (y + r2) 0)
          GL.renderPrimitive GL.Polygon (circle r1 r2 0 (2 * pi) (6 / (r1 + r2)))
  in [[Pos ("E" ++ show (x, y, width, height), drawing)]]

createPolygon :: [Point] -> Region
createPolygon [] = [[]]
createPolygon ps =
  let (minx, maxx, miny, maxy) = (minimum (map fst ps), maximum (map fst ps),
                                  minimum (map snd ps), maximum (map snd ps))
      drawing = do
        GL.renderPrimitive GL.Polygon (foldr1 (>>) (map 
          (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0))
          ps))
   in [[Pos ("P"++show ps, drawing)]]

andRegion, orRegion, xorRegion, diffRegion :: Region -> Region -> Region

-- We'll convert region expression into disjuction canonical form
-- so as to make rendering easier using Stencil buffer.

type Region = [Conjuction]
type Conjuction = [Atom]
data Atom = Pos Atom' | Neg Atom'
type Atom' = (String, IO ())
instance Show Atom where
  show (Pos (s, _)) = "+" ++ s
  show (Neg (s, _)) = "-" ++ s

conjuction :: Region -> Region -> Region
conjuction xs ys = [ x ++ y | x <- xs, y <- ys ]
disjuction xs ys = xs ++ ys
negTerm [] = []
negTerm xs = foldl1 conjuction (map negA xs)
  where
    negA :: Conjuction -> Region 
    negA ys = map negS ys
    negS :: Atom -> Conjuction
    negS (Pos i) = [Neg i]
    negS (Neg i) = [Pos i]

data RegionOp = AND | OR | XOR | DIFF

andRegion  = combineRegion AND
orRegion   = combineRegion OR
xorRegion  = combineRegion XOR
diffRegion = combineRegion DIFF

drawRegion :: Region -> Graphic
drawRegion term = Graphic drawAux
  where
    drawAux = do
      GL.stencilMask $= 1
      GL.stencilTest $= GL.Enabled
      sequence_ [drawConjuction (posT t) (negT t) | t <- term]
      GL.stencilTest $= GL.Disabled

    posT [] = []
    posT (Pos x:xs) = x : posT xs
    posT (_:xs) = posT xs

    negT [] = []
    negT (Neg x:xs) = x : negT xs
    negT (_:xs) = negT xs

    drawConjuction ps ns = do
      -- render all positive atoms only to stencil buffer
      GL.depthFunc $= Just GL.Never
      GL.stencilMask $= 0xff
      GL.stencilFunc $= (GL.Greater, 0, 0xff)
      -- every pixel rendered increases the value in the stencil buffer by 1
      GL.stencilOp $= (GL.OpIncr, GL.OpIncr, GL.OpZero)
      mapM_ drawIt ps
      -- render all negative atoms to clear the stencil pixel to 0
      GL.stencilOp $= (GL.OpZero, GL.OpZero, GL.OpZero)
      mapM_ drawIt ns
      -- finally render all positive atoms to screen where the stencil pixel
      -- equals (length ps)
      GL.depthFunc $= Just GL.Always
      GL.stencilFunc $= (GL.Equal, fromIntegral $ length ps, 0xff)
      GL.stencilOp $= (GL.OpZero, GL.OpZero, GL.OpZero)
      mapM_ drawIt ps

    drawIt (_, io) = io

--combineRegion :: Cairo.Operator -> Region -> Region -> Region
combineRegion operator a b =
  case operator of
    AND -> conjuction a b
    OR -> disjuction a b
    XOR -> disjuction (conjuction (negTerm a) b) (conjuction a (negTerm b))
    DIFF -> conjuction a (negTerm b)

-}
---------------------------
-- Event Handling Functions
---------------------------

data KeyModifiers = KeyModifiers {
    shift :: Bool,
    ctrl  :: Bool,
    alt   :: Bool }
  deriving (Show, Eq)

data UIEvent = 
    Key {
      key :: GLFW.Key,
      isDown :: Bool,
      modifiers :: KeyModifiers
    }
  | Character Char -- Character events in GLFW only emit when pressed.
  | Button {
     pt :: Point,
     isLeft :: Bool,
     isDown :: Bool
    }
  | MouseMove {
      pt :: Point
    }
  | Resize GL.Size
  | Refresh
  | Closed
  | NoUIEvent
 deriving Show


-- | getWindowEvent and maybeGetWindowEvent both take an additional argument 
--  sleepTime that tells how long to sleep in the case where there are no
--  window events to return.  This is used to allow the cpu to take other 
--  tasks at these times rather than needlessly spinning.  The sleepTime 
--  parameter used to be fixed at 0.01.

getWindowEvent :: Double -> Window -> IO UIEvent
getWindowEvent sleepTime win = do
  event <- maybeGetWindowEvent sleepTime win
  maybe (getWindowEvent sleepTime win) return event

maybeGetWindowEvent :: Double -> Window -> IO (Maybe UIEvent)
maybeGetWindowEvent sleepTime win = let winChan = eventsChan win in do
  updateWindowIfDirty win
  mevent <- atomically $ tryReadTChan winChan
  case mevent of
    Nothing -> GLFW.sleep sleepTime >> GLFW.pollEvents >> return Nothing
    Just Refresh -> do
      (Graphic io, _) <- readMVar (graphicVar win)
      io
      GLFW.swapBuffers
      maybeGetWindowEvent sleepTime win
    Just (e@(Resize _)) -> do
      (Resize size@(GL.Size w h)) <- getLastResizeEvent winChan e
      GL.viewport $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
      -- force a refresh, needed for OS X
      atomically $ writeTChan winChan Refresh
      maybeGetWindowEvent sleepTime win
    Just e -> return (Just e)

-- | When a window is resized, all of the resize events queue up until the 
--   mouse button is released.  This causes some delay as each individual 
--   resize event is handled and then the window is redrawn.  This function 
--   clears all resize and refresh events until the last resize one.
--   Note that because this function is used, a Refresh event should follow 
--   the resizing.
getLastResizeEvent :: TChan UIEvent -> UIEvent -> IO UIEvent
getLastResizeEvent ch prev = do
    mevent <- atomically $ tryReadTChan ch
    case mevent of
      Nothing -> return prev
      Just (e@(Resize _)) -> getLastResizeEvent ch e
      Just Refresh        -> getLastResizeEvent ch prev
      Just e -> atomically (unGetTChan ch e) >> return prev


-- | getKeyEx, getKey, getButton, getLBP, and getRBP are defined here but 
--  never used in Euterpea.  Furthermore, due to the change in getWindowEvent 
--  so that it now requires a sleepTime argument (previously fixed at 0.01), 
--  they either need to be parameterized over sleepTime or set.  I'm not 
--  sure which is the better solution, so I will leave them commented out 
--  until they're needed.

{-
getKeyEx :: Window -> Bool -> IO Char
getKeyEx win down = loop
  where loop = do e <- getWindowEvent win
                  case e of
                    (Key { char = ch, isDown = d })
                      | d == down -> return ch
                    Closed -> return '\x0'
                    _ -> loop

getKey :: Window -> IO Char
getKey win = do
  ch <- getKeyEx win True
  if ch == '\x0' then return ch
    else getKeyEx win False

getButton :: Window -> Int -> Bool -> IO Point
getButton win but down = loop
  where loop = do e <- getWindowEvent win
                  case e of
                    (Button { pt = pt, isDown = id })
                      | id == down -> return pt
                    _ -> loop

getLBP :: Window -> IO Point
getLBP w = getButton w 1 True

getRBP :: Window -> IO Point
getRBP w = getButton w 2 True
-}

-- use GLFW's high resolution timer
timeGetTime :: IO Double
timeGetTime = GL.get GLFW.time

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

-- Designed to be used with GLFW.Key, GLFW.CharKey, or GLFW.SpecialKey
isKeyPressed :: Enum a => a -> IO Bool
isKeyPressed k = do
    kbs <- GLFW.getKey k
    return $ case kbs of
        GLFW.Press   -> True
        GLFW.Release -> False

----------------------
-- Auxiliary Functions
----------------------

vertex4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL.Vertex4 GLfloat
vertex4 = GL.Vertex4

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

normaliseBounds :: Point -> Point -> (GLfloat,GLfloat,GLfloat,GLfloat)
normaliseBounds (x1,y1) (x2,y2) = (x, y, width, height)
  where x = fromIntegral $ min x1 x2
        y = fromIntegral $ min y1 y2
        width  = fromIntegral $ abs $ x1 - x2
        height = fromIntegral $ abs $ y1 - y2

normaliseBounds' :: Point -> Point -> (Int,Int,Int,Int)
normaliseBounds' (x1,y1) (x2,y2) = (x, y, width, height)
  where x = min x1 x2
        y = min y1 y2
        width  = abs $ x1 - x2
        height = abs $ y1 - y2

fromPoint :: Point -> (GLfloat, GLfloat)
fromPoint (x1, x2) = (fromIntegral x1, fromIntegral x2)

fromSize (x, y) = GL.Size (fromIntegral x) (fromIntegral y)

-- we add 20 pixels to the y position to leave space for window title bar
fromPosition (x, y) = GL.Position (fromIntegral x) (20 + fromIntegral y)

circle r1 r2 start stop step =
  let vs = [ (r1 * cos i, r2 * sin i) | i <- segment start stop step ]
  in mapM_ (\(x, y) -> GL.vertex (vertex3 x y 0)) vs

segment start stop step = ts start
  where ts i = if i >= stop then [stop] else (i : ts (i + step))

