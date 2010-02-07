module Render
       ( getAspectRatio
       , getRenderFunction
       ) where

import Control.Monad
import Data.Array
import Data.Char
import Data.IORef
import Foreign.Marshal
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL hiding (position)

import Actor as A
import Game
import GraphUtils
import Level
import Text

hudHeight = 0.2

solid :: GLfloat
solid = 1

fullLevelSize = addBorder . levelSize
  where addBorder (w,h) = (w+2,h+2)

getAspectRatio level = fromIntegral lh / fromIntegral lw + hudHeight
  where (lw,lh) = fullLevelSize level

getRenderFunction charset = do
  rgbOverlay <- createTexture 24 24 True $ flip pokeArray $
                concat [if b then [95,95,95,255] else c | y <- [0..23], x <- [0..23],
                        let c = case x `div` 4 `mod` 3 of
                              0 -> [255,191,191,255]
                              1 -> [191,255,191,255]
                              2 -> [191,191,255,255]
                            b = x `mod` 4 == 0 || x < 12 && y `mod` 12 == 0 || x >= 12 && y `mod` 12 == 6
                       ]

  return $ render (uncurry displayString charset) rgbOverlay

render displayText rgbOverlay levelState levelCount score1 score2 = do
  let curLevel = level levelState
      (lw,lh) = fullLevelSize curLevel
      height = fromIntegral lh / fromIntegral lw
      magn = 1 / fromIntegral lw

  clear [ColorBuffer]
  loadIdentity

  renderHud displayText height score1 score2 (actors levelState) levelCount curLevel
  preservingMatrix $ do
    translate $ Vector3 0 hudHeight (0 :: GLfloat)
    scale magn magn (1 :: GLfloat)

    renderLevel curLevel
    renderBullets (bullets levelState)
    renderActors (actors levelState)

  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just rgbOverlay
  blendFunc $= (Zero,SrcColor)
  let pscale = 100
  renderPrimitive Quads $ do
    texCoord2 0 0
    vertex3 0 0 0
    texCoord2 pscale 0
    vertex3 1 0 0
    texCoord2 pscale pscale
    vertex3 1 1 0
    texCoord2 0 pscale
    vertex3 0 1 0
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)

  flush
  swapBuffers

renderLevel level = do
  let (lw,lh) = fullLevelSize level
      height = fromIntegral lh / fromIntegral lw
      entries = [((y,x),[]) | (_,y,x) <- entrances level]

  texture Texture2D $= Disabled
  color $ Color4 0.2 0.5 1 solid
  forM_ (entries ++ assocs (legalMoves level)) $ \((y,x),ms) -> do
    let xc = fromIntegral (x+1)
        yc = fromIntegral (lh-y-2)
    unless (North `elem` ms) $ drawRectangle xc (yc+0.95) 1 0.05
    unless (South `elem` ms) $ drawRectangle xc yc 1 0.05
    unless (East `elem` ms) $ drawRectangle (xc+0.95) yc 0.05 1
    unless (West `elem` ms) $ drawRectangle xc yc 0.05 1

renderActors as = do
  texture Texture2D $= Enabled
  color $ Color4 1 1 1 solid
  forM_ as $ \a -> do
    let V x y = A.position a
        x' = fromIntegral x / fromIntegral fieldSize
        y' = fromIntegral y / fromIntegral fieldSize
    when (not . null . animation $ a) $ drawSprite (head (animation a)) (1.1+x') (1.1+y') 0.8 0.8 (facing a)

renderBullets bs = do
  texture Texture2D $= Disabled
  color $ Color4 1 0.3 0.1 solid
  forM_ bs $ \(_,V x y) -> do
    let x' = fromIntegral x / fromIntegral fieldSize
        y' = fromIntegral y / fromIntegral fieldSize
    drawRectangle (1.45+x') (1.45+y') 0.1 0.1

renderHud displayText height score1 score2 actors levelCount level = do
  let r = Color4 1 0 0 solid
      b = Color4 0.25 0.63 1 solid
      y = Color4 0.93 0.79 0 solid
      d = Color4 0 0 0 solid
      charSize = 0.0028
      radarTitle = if levelCount == 1 then "RADAR" else
                     case levelName level of
                       "pit"   -> "THE PIT"
                       "arena" -> "THE ARENA"
                       _       -> "DUNGEON " ++ show levelCount
      (levelW,levelH) = levelSize level
      scoreB = hudHeight / 4
      scoreW = 0.3
      scoreH = scoreB*3
      radarB = charSize
      radarW = radarF*fromIntegral levelW
      radarH = hudHeight-radarB*4-charSize*10
      radarF = radarH/fromIntegral levelH
      num1 = show score1
      num2 = show score2

  texture Texture2D $= Disabled
  color b
  drawRectangle 0 0 scoreW scoreH
  drawRectangle (0.5-radarW/2-radarB) 0 (radarW+radarB*2) (radarH+radarB*2)
  color y
  drawRectangle (1-scoreW) 0 scoreW scoreH
  color d
  drawRectangle scoreB scoreB (scoreW-2*scoreB) (scoreH-2*scoreB)
  drawRectangle (1-scoreW+scoreB) scoreB (scoreW-2*scoreB) (scoreH-2*scoreB)
  drawRectangle (0.5-radarW/2) radarB radarW radarH
  forM_ actors $ \actor -> do
    let (fx,fy) = fieldPos (position actor)
        rx = 0.5-radarW/2+radarF*fromIntegral fx
        ry = radarB+radarF*fromIntegral fy
    case actorType actor of
      Burwor -> color b >> drawRectangle rx ry radarF radarF
      Garwor -> color y >> drawRectangle rx ry radarF radarF
      Thorwor -> color r >> drawRectangle rx ry radarF radarF
      _ -> return ()

  texture Texture2D $= Enabled
  color y
  displayText (1-scoreB-(fromIntegral (length num1)*8*charSize)) (scoreB+scoreH/2-scoreB-5*charSize) charSize num1
  color b
  displayText (scoreW-scoreB-(fromIntegral (length num2)*8*charSize)) (scoreB+scoreH/2-scoreB-5*charSize) charSize num2
  color r
  displayText (0.5-(fromIntegral (length radarTitle)*4*charSize)) (radarH+radarB*3) charSize radarTitle

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
