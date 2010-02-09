module Render
       ( getAspectRatio
       , getRenderFunctions
       , setViewport
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
import Vector

hudHeight = 0.2

solid :: GLfloat
solid = 1

fullLevelSize = addBorder . levelSize
  where addBorder (w,h) = (w+2,h+2)

getAspectRatio level = fromIntegral lh / fromIntegral lw + hudHeight
  where (lw,lh) = fullLevelSize level

setViewport ref size@(Size w h) = do
  lr <- readIORef ref

  let r = (fromIntegral h/fromIntegral w)
      r' = recip r
      lr' = recip lr
      s = 2*min (max 1 (min lr' r')) (max (r/lr) lr')

  viewport $= (Position 0 0,size)

  matrixMode $= Projection
  loadIdentity
  scale (s*min 1 r) (s*min 1 r') (1 :: GLfloat)
  translate $ Vector3 (-0.5) (-0.5*lr) (0 :: GLfloat)

  matrixMode $= Modelview 0

setAspectRatio ref val = do
  writeIORef ref val
  setViewport ref =<< get windowSize

getRenderFunctions aspectRatio charset = do
  let displayText = uncurry displayString charset

  rgbOverlay <- createTexture 24 24 True $ flip pokeArray $
                concat [if b then [95,95,95,255] else c | y <- [0..23], x <- [0..23],
                        let c = case x `div` 4 `mod` 3 of
                              0 -> [255,191,191,255]
                              1 -> [191,255,191,255]
                              2 -> [191,191,255,255]
                            b = x `mod` 4 == 0 || x < 12 && y `mod` 12 == 0 || x >= 12 && y `mod` 12 == 6
                       ]

  return (renderGame aspectRatio displayText rgbOverlay
         ,renderMenu aspectRatio displayText rgbOverlay
         )

renderMenu aspectRatio displayText rgbOverlay items item = do
  let charSize = 0.006
      textCol = Color4 1 0 0 solid
      activeCol = Color4 0.93 0.79 0 solid

  setAspectRatio aspectRatio 1

  clear [ColorBuffer]
  loadIdentity

  texture Texture2D $= Enabled
  forM_ (zip ("DUNGEONS OF WOR":"":"":items) [-3..]) $ \(text,i) -> do
    color $ if i == item then activeCol else textCol
    displayText (0.5-(fromIntegral (length text)*4*charSize))
                (0.5+charSize*(7*fromIntegral (length items-1-2*i)))
                charSize text

  renderOverlay rgbOverlay

  flush
  swapBuffers

renderGame aspectRatio displayText rgbOverlay levelState levelCount scores lives = do
  let curLevel = level levelState
      (lw,lh) = fullLevelSize curLevel
      height = fromIntegral lh / fromIntegral lw
      magn = 1 / fromIntegral lw

  setAspectRatio aspectRatio (getAspectRatio curLevel)

  clear [ColorBuffer]
  loadIdentity

  renderHud displayText height scores lives (actors levelState) levelCount curLevel
  preservingMatrix $ do
    translate $ Vector3 0 hudHeight (0 :: GLfloat)
    scale magn magn (1 :: GLfloat)

    renderLevel curLevel
    renderBullets (bullets levelState)
    renderActors (actors levelState)

  renderOverlay rgbOverlay

  flush
  swapBuffers

renderOverlay rgbOverlay = do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just rgbOverlay
  blendFunc $= (Zero,SrcColor)
  color $ Color4 1 1 1 solid
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

renderHud displayText height scores lives actors levelCount level = do
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
      snum1 = show (scores !! 0)
      snum2 = show (scores !! 1)
      lnum1 = show (lives !! 0)
      lnum2 = show (lives !! 1)
      numPlayers = length lives

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
  displayText (1-scoreB-(fromIntegral (length snum1)*8*charSize)) (scoreB+scoreH/2-scoreB-5*charSize) charSize snum1
  displayText (1-scoreW-3*radarB-(fromIntegral (length lnum1)*8*charSize)) (scoreB+scoreH/2-scoreB-5*charSize) charSize lnum1
  when (numPlayers > 1) $ do
    color b
    displayText (scoreW-scoreB-(fromIntegral (length snum2)*8*charSize)) (scoreB+scoreH/2-scoreB-5*charSize) charSize snum2
    displayText (scoreW+3*radarB) (scoreB+scoreH/2-scoreB-5*charSize) charSize lnum2
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
