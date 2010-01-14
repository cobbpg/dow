{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.IORef
import FRP.Elerea.Experimental.Simple
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

import Actor
import Game
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
  let level = levels !! 4
      skins = createSkins sprites
      newActor = mkActor skins

  aspectRatio <- newIORef (getAspectRatio level)
  windowSizeCallback $= resizeWindow aspectRatio

  closed <- newIORef False
  windowCloseCallback $= writeIORef closed True

  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  textureFilter Texture2D $= ((Linear',Just Nearest),Linear')
  textureFunction $= Combine4

  (keyPress,keySink) <- external (False,False,False,False)
  renderAction <- start $ game (render displayText level) newActor level keyPress

  fix $ \loop -> do
    readKeys keySink
    join renderAction
    sleep 0.02
    stop <- readIORef closed
    esc <- getKey ESC
    when (not stop && esc /= Press) loop

  closeWindow

readKeys sink = do
  let pr = (==Press)
  kn <- getKey UP
  ks <- getKey DOWN
  kw <- getKey LEFT
  ke <- getKey RIGHT
  sink (pr kn, pr ks, pr kw, pr ke)

game renderFun newActor level keyPress = mdo
  player <- transfer (newActor YellowWorrior (V 0 0)) (movePlayer level) (keyDir <$> keyPress)
  enemyInputs <- replicateM 6 $ latch East (intDir <$> noise)
  enemies <- mapM (transfer ((newActor Burwor (V fieldSize 0)) { speed = 4 }) (moveEnemy level)) enemyInputs
  return $ renderFun <$> sequence (player:enemies)

latch x0 s = transfer x0 store s
    where store Nothing  x = x
          store (Just x) _ = x

keyDir (True,_,_,_) = Just North
keyDir (_,True,_,_) = Just South
keyDir (_,_,True,_) = Just West
keyDir (_,_,_,True) = Just East
keyDir _            = Nothing

intDir x = if x `mod` 1000 < 950 then Nothing else Just (toEnum (x `mod` 4))

movePlayer level Nothing    plr = plr
movePlayer level (Just dir) plr = animate (move level dir plr)

moveEnemy level dir act = animate (move level dir act)

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
