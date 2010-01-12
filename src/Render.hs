module Render where

import Control.Monad
import Data.Array
import Data.IORef
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

import Level

hudHeight = 0.2

solid :: GLfloat
solid = 1

getAspectRatio level = fromIntegral lh / fromIntegral lw + hudHeight
  where (lw,lh) = levelSize level

render level = do
  let (lw,lh) = levelSize level
      height = fromIntegral lh / fromIntegral lw

  clear [ColorBuffer]
  loadIdentity

  renderLevel level
  renderHud height

  flush
  swapBuffers

renderLevel level = preservingMatrix $ do
  let (lw,lh) = levelSize level
      height = fromIntegral lh / fromIntegral lw
      magn = 1 / fromIntegral lw

  translate $ Vector3 0 hudHeight (0 :: GLfloat)
  scale magn magn (1 :: GLfloat)

  color $ Color4 0.1 0.1 0.6 solid
  forM_ (assocs (legalMoves level)) $ \((y,x),ms) -> do
    let xc = fromIntegral x
        yc = fromIntegral (lh-y-1)
    unless (North `elem` ms) $ drawRectangle xc (yc+0.95) 1 0.05
    unless (South `elem` ms) $ drawRectangle xc yc 1 0.05
    unless (East `elem` ms) $ drawRectangle (xc+0.95) yc 0.05 1
    unless (West `elem` ms) $ drawRectangle xc yc 0.05 1

renderHud height = do
  color $ Color4 0.6 0.6 0.6 solid
  drawRectangle 0 0 1 hudHeight

drawRectangle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRectangle x y sx sy = renderPrimitive Quads $ mapM_ vertex
  [Vertex3 x y 0, Vertex3 (x+sx) y 0, Vertex3 (x+sx) (y+sy) 0, Vertex3 x (y+sy) 0]
