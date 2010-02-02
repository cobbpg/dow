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
import GraphUtils
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
  charset <- loadCharset "charset.txt"
  render <- getRenderFunction charset

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
  renderAction <- start $ game render newActor levels keyPress

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
  let startLevel level enemyCount = playLevel newActor keyPress level enemyCount
      pickLevel cnt rnd = case () of
        _ | cnt == 4         -> findLevel (=="arena")
        _ | cnt < 8          -> findLevel ((=='b').head)
        _ | cnt `mod` 6 == 1 -> findLevel (=="pit")
        otherwise            -> findLevel ((=='w').head)
        where findLevel p = pickOne (filter (p.levelName) levels)
              pickOne xs = xs !! (rnd `mod` length xs)
      accumScore state prev = prev + (foldl' (+) 0 . map killScore . tail . actors) state
        where killScore Actor { action = Dying, animation = [_], tick = 0, actorType = t } =
                case t of
                  Burwor -> 100
                  Garwor -> 200
                  Thorwor -> 500
                  Worluk -> 1000
                  _ -> 0
              killScore _ = 0

  levelNoise <- noise
  (state,trig) <- switcher (startLevel <$> (pickLevel <$> levelCount <*> levelNoise) <*> levelCount)
  trig' <- delay False trig
  levelCount <- transfer 1 (\win cnt -> if win then cnt+1 else cnt) trig'
  score <- transfer 0 accumScore state

  return (renderFun <$> state <*> levelCount <*> score)

playLevel newActor keyPress level enemyCount = mdo
  let mkEnemy etype = enemy (newActor etype) level bullets

      mkShot c plr = if c && isAlive plr
                     then (:[]) <$> bullet (position plr) (facing plr)
                     else return []

      spawnEnemies = concatMap spawnEnemy
      spawnEnemy enemy = if isDead enemy && length (animation enemy) == 1 && tick enemy == 0 then
                           case actorType enemy of
                             Burwor -> [mkEnemy Garwor <$> (position <$> player)]
                             Garwor -> [mkEnemy Thorwor <$> (position <$> player)]
                             _      -> []
                         else []

  shoot <- memo =<< edge (keyShoot <$> keyPress)
  (player,playerDeath) <- switcher . pure $ do
    plr <- transfer3 (newActor YellowWorrior (V 0 0)) (movePlayer level) (keyDir <$> keyPress) shoot enemies'
    return (plr, null . animation <$> plr)

  bulletSource <- generator (mkShot <$> shoot <*> player)
  bullets <- collection bulletSource (notHitAnything level <$> enemies')

  initialEnemies <- replicateM enemyCount (mkEnemy Burwor (V 0 0))
  spawnedEnemies <- generator (sequence <$> (sequence . spawnEnemies =<< enemies'))
  enemySource <- delay initialEnemies spawnedEnemies
  enemies <- collection enemySource (pure (not . null . animation))
  enemies' <- delay [] enemies

  return (LevelState level <$> liftA2 (:) player enemies <*> bullets
         ,null <$> enemies
         )

bullet pos dir = stateful pos (+3*dirVec dir)

enemy newActor level bullets (V playerX playerY) = mdo
  let actorInit = (newActor (fromIntegral fieldSize*startPos)) { speed = 4 }
      startPos = if abs (startX-playerX `div` fieldSize) < 3 && abs (startY-playerY `div` fieldSize) < 3
                 then V ((startX + lw `div` 2) `mod` lw) ((startY + lh `div` 2) `mod` lh)
                 else V startX startY
      (lw,lh) = levelSize level

  startX <- (`mod` lw) <$> getRandom
  startY <- (`mod` lh) <$> getRandom
  startDir <- toEnum . (`mod` 4) <$> getRandom
  dirNoise <- noise

  actorInput <- startDir --> newDir level <$> dirNoise <*> actor'
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
  Just dir -> animate (if isAlive plr then move level dir plr' else plr')
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
        killed = isJust (find (==getXY plr) [getXY enemy | enemy <- enemies, isAlive enemy])
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
