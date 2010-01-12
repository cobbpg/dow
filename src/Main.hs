module Main where

import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

import Level
import Render

main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "Wizard of Wor"

  let Right (level,_) = parseLevel testLevel

  aspectRatio <- newIORef (getAspectRatio level)
  windowSizeCallback $= resizeWindow aspectRatio

  closed <- newIORef False
  windowCloseCallback $= writeIORef closed True

  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)

  fix $ \loop -> do
    render level
    stop <- readIORef closed
    when (not stop) loop

  closeWindow

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

testLevel =
  ["\"01\""
  ,"+-----+---------+-----+"
  ,"|     |         |     |"
  ,"| +-+ + +-+ +-+ + +-+ |"
  ,"| |                 | |"
  ,"+ + + +---+ +---+ + + +"
  ,"<   |     | |     |   >"
  ,"+   | +-+ + + +-+ |   +"
  ,"|   | |         | |   |"
  ,"| +-+ + +-+ +-+ + +-+ |"
  ,"| |       | |       | |"
  ,"| + +-+ + + + + +-+ + |"
  ,"|       |     |       |"
  ,"+*+-----+-----+-----+*+"
  ]
