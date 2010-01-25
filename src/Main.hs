{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import FRP.Elerea.Experimental.Simple
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL hiding (position)

import Actor
import Game
import Level
import Render
import Sprites
import Text
import Utils

main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "Dungeons of Wor"

  levels <- loadLevels "levels.txt"
  sprites <- loadSprites "sprites.txt"
  displayText <- uncurry displayString <$> loadCharset "charset.txt"
  let skins = createSkins sprites
      newActor = mkActor skins

  aspectRatio <- newIORef (getAspectRatio (levels !! 0))
  windowSizeCallback $= resizeWindow aspectRatio

  closed <- newIORef False
  windowCloseCallback $= writeIORef closed True

  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  textureFilter Texture2D $= ((Linear',Just Nearest),Linear')
  textureFunction $= Combine4

  (keyPress,keySink) <- external (False,False,False,False,False)
  renderAction <- start $ game (render displayText) newActor levels keyPress

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
  kt <- getKey LCTRL
  sink (pr kn, pr ks, pr kw, pr ke, pr kt)

game renderFun newActor levels keyPress = mdo
  let startLevel level = playLevel newActor keyPress level
      pickLevel cnt rnd = case () of
        _ | cnt == 4         -> findLevel (=="arena")
        _ | cnt < 8          -> findLevel ((=='b').head)
        _ | cnt `mod` 6 == 1 -> findLevel (=="pit")
        otherwise            -> findLevel ((=='w').head)
        where findLevel p = pickOne (filter (p.levelName) levels)
              pickOne xs = xs !! (rnd `mod` length xs)
  (state,trig) <- switcher False (startLevel <$> (pickLevel <$> levelCount <*> noise))
  trig' <- delay Nothing trig
  levelCount <- transfer 1 (\win cnt -> if win == Just True then cnt+1 else cnt) trig'
  --levelCount' <- delay 1 levelCount
  return (renderFun <$> state <*> levelCount)

playLevel newActor keyPress level = mdo
  let mkTrigger enemies player = if null enemies || null (animation player)
                                 then Just (not (isDead player))
                                 else Nothing

  shoot <- memo =<< edge (keyShoot <$> keyPress)
  player <- transfer3 (newActor YellowWorrior (V 0 0)) (movePlayer level) (keyDir <$> keyPress) shoot enemies'

  bulletSource <- generator (mkShot <$> shoot <*> player)
  bullets <- collection bulletSource (notHitAnything level <$> enemies')

  enemySource <- flip delay (pure []) =<< replicateM 2 (enemy newActor level bullets)
  enemies <- collection enemySource (pure (not . null . animation))
  enemies' <- delay [] enemies

  return (LevelState level <$> liftA2 (:) player enemies <*> bullets
         ,mkTrigger <$> enemies <*> player
         )

mkShot c plr = if c && not (isDead plr)
               then (:[]) <$> bullet (position plr) (facing plr)
               else return []

bullet pos dir = stateful pos (+3*dirVec dir)

enemy newActor level bullets = mdo
  let actorInit = (newActor Burwor (V (fieldSize*(lw-1)) (fieldSize*(lh-1)))) { speed = 4 }
      (lw,lh) = levelSize level
  actorInput <- latch East (newDir level <$> noise <*> actor')
  actor <- transfer2 actorInit (moveEnemy level) actorInput bullets
  actor' <- delay actorInit actor
  return actor

notHitAnything lev es pos@(V px py) =
  (sy <  fieldMid-bs || North `elem` legals) &&
  (sx <  fieldMid-bs || East  `elem` legals) &&
  (sy > -fieldMid+bs || South `elem` legals) &&
  (sx > -fieldMid+bs || West  `elem` legals) &&
  not hitExplosion
  where legals = legalMovesAt lev (fieldPos pos)
        (sx,sy) = fieldSub pos
        bs = 3

        hitExplosion = any hitBy es
        hitBy e = action e == Dying && abs (ex-px) < fieldMid && abs (ey-py) < fieldMid
          where V ex ey = position e

latch x0 s = transfer x0 store s
    where store Nothing  x = x
          store (Just x) _ = x

keyDir (True,_,_,_,_) = Just North
keyDir (_,True,_,_,_) = Just South
keyDir (_,_,True,_,_) = Just West
keyDir (_,_,_,True,_) = Just East
keyDir _              = Nothing

keyShoot (_,_,_,_,s) = s

newDir lev rnd act = if not (canMove lev act) then Just pickedLegalDir
                     else if rnd `mod` 1000 < 992 then Nothing
                          else Just (toEnum (rnd `mod` 4))
  where legal = legalMovesAt lev (fieldPos (position act))
        pickedLegalDir = legal !! (rnd `mod` length legal)

movePlayer level mov shoot enemies plr = case mov of
  Nothing  -> if action plr' /= Walking then animate plr' else plr'
  Just dir -> animate (if not (isDead plr) then move level dir plr' else plr')
  where plr' = case () of
          _ | isDead plr -> plr
          _ | killed     -> plr { animation = deathAnimation (skin plr)
                                , action = Dying
                                , speed = 6
                                }
          _ | shoot      -> plr { animation = shootAnimation (skin plr)
                                , action = Shooting
                                }
          otherwise      -> plr
        killed = isJust (find (==getXY plr) [getXY enemy | enemy <- enemies, not (isDead enemy)])
        getXY = fieldPos.position

moveEnemy level dir bs act = animate (mv act')
  where act' = if hit then
                 act { animation = deathAnimation (skin act)
                     , action = Dying
                     }
               else act
        mv = if action act' /= Dying then move level dir else id
        hit = any hitBy bs
        V px py = position act
        hitBy (V bx by) = abs (bx-px) < fieldMid && abs (by-py) < fieldMid

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
