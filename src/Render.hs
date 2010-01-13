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

render displayText level sprites = do
  let (lw,lh) = levelSize level
      height = fromIntegral lh / fromIntegral lw
      magn = 1 / fromIntegral lw

  clear [ColorBuffer]
  loadIdentity

  renderHud height
  preservingMatrix $ do
    translate $ Vector3 0 hudHeight (0 :: GLfloat)
    scale magn magn (1 :: GLfloat)

    texture Texture2D $= Disabled
    renderLevel level
    texture Texture2D $= Enabled
    color $ Color4 1 1 1 solid
    forM_ (zip sprites [0..]) $ \(s,i) -> do
      drawSprite (head (snd s)) (0.1+i) 0.1 0.8 0.8 North
      drawSprite (head (snd s)) (0.1+i) 1.1 0.8 0.8 East
      drawSprite (head (snd s)) (0.1+i) 2.1 0.8 0.8 South
      drawSprite (head (snd s)) (0.1+i) 3.1 0.8 0.8 West

  displayText 0.03 0.05 0.003 "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 0123456789"

  flush
  swapBuffers

renderLevel level = do
  let (lw,lh) = levelSize level
      height = fromIntegral lh / fromIntegral lw

  color $ Color4 0.2 0.5 1 solid
  forM_ (assocs (legalMoves level)) $ \((y,x),ms) -> do
    let xc = fromIntegral x
        yc = fromIntegral (lh-y-1)
    unless (North `elem` ms) $ drawRectangle xc (yc+0.95) 1 0.05
    unless (South `elem` ms) $ drawRectangle xc yc 1 0.05
    unless (East `elem` ms) $ drawRectangle (xc+0.95) yc 0.05 1
    unless (West `elem` ms) $ drawRectangle xc yc 0.05 1

renderHud height = do
  texture Texture2D $= Disabled
  color $ Color4 0.6 0.6 0.6 solid
  drawRectangle 0 0 1 hudHeight

drawRectangle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRectangle x y sx sy = renderPrimitive Quads $ mapM_ vertex
  [Vertex3 x y 0, Vertex3 (x+sx) y 0, Vertex3 (x+sx) (y+sy) 0, Vertex3 x (y+sy) 0]

drawSprite :: TextureObject -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Direction -> IO ()
drawSprite tid x y sx sy dir = do
  let (u1,v1,u2,v2,u3,v3,u4,v4) = case dir of
        North -> (1,1,1,0,0,0,0,1)
        East  -> (1,1,0,1,0,0,1,0)
        South -> (0,1,0,0,1,0,1,1)
        West  -> (0,1,1,1,1,0,0,0)
  textureBinding Texture2D $= Just tid
  renderPrimitive Quads $ do
    texCoord2 u1 v1
    vertex3 x y 0
    texCoord2 u2 v2
    vertex3 (x+sx) y 0
    texCoord2 u3 v3
    vertex3 (x+sx) (y+sy) 0
    texCoord2 u4 v4
    vertex3 x (y+sy) 0

vertex3 x y z = vertex $ Vertex3 x y (z :: GLfloat)

texCoord2 x y = texCoord $ TexCoord2 x (y :: GLfloat)
