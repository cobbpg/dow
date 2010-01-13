module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.IORef
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

import Level
import Render
import Sprites
import Text

main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "Wizard of Wor"

  levels <- loadLevels "levels.txt"
  sprites <- loadSprites "sprites.txt"
  displayText <- uncurry displayString <$> loadCharset "charset.txt"
  let level = levels !! 2

  aspectRatio <- newIORef (getAspectRatio level)
  windowSizeCallback $= resizeWindow aspectRatio

  closed <- newIORef False
  windowCloseCallback $= writeIORef closed True

  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  textureFilter Texture2D $= ((Linear',Just Nearest),Linear')
  textureFunction $= Combine4

  fix $ \loop -> do
    render displayText level sprites
    stop <- readIORef closed
    when (not stop) loop

  closeWindow

loadLevels file = do
  dat <- lines <$> readFile file
  let levels = parseLevels dat
      parseLevels dat = case parseLevel dat of
        Left _ -> []
        Right (l,dat') -> l : parseLevels dat'

  return levels

loadSprites file = do
  dat <- lines <$> readFile file
  parseSprites dat

trim s = reverse . dropWhile isSpace . reverse . dropWhile isSpace

resizeWindow ref size@(Size w h) = do
  lr <- readIORef ref

  let r = fromIntegral h/fromIntegral w
      r' = recip r
      lr' = recip lr
      s = 2*min (max 1 (min lr' r')) (max (r/lr) lr')

  viewport $= (Position 0 0,size)

  matrixMode $= Projection
  loadIdentity
  scale (s*min 1 r) (s*min 1 r') (1 :: GLfloat)
  translate $ Vector3 (-0.5) (-0.5*lr) (0 :: GLfloat)

  matrixMode $= Modelview 0
