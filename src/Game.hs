{-# LANGUAGE RecursiveDo #-}

module Game where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Array
import Data.List
import Data.Maybe
import FRP.Elerea.Experimental.Simple

import Actor
import HighScore
import Level
import Sprites
import Utils
import Vector

numberOfLives = 5

data LevelState = LevelState
                  { level :: Level
                  , actors :: [Actor]
                  , bullets :: [(ActorType,Vec)]
                  , shooters :: [ActorType]
                  }

keySet1 = fst
keySet2 = snd

keyDir (True,_,_,_,_) = Just North
keyDir (_,True,_,_,_) = Just South
keyDir (_,_,True,_,_) = Just West
keyDir (_,_,_,True,_) = Just East
keyDir _              = Nothing

keyShoot (_,_,_,_,s) = s

keyAny k = keyShoot k || isJust (keyDir k)

game highScore storeScore (renderGame,renderMenu) closeAction newActor levels keyPress = do
  let firstTrue s = do
        mask <- delay False =<< transfer False (||) s
        return (liftA2 (&&) (not <$> mask) s)

      mkGame 0 = playGame storeScore renderGame newActor levels keyPress 1
      mkGame 1 = playGame storeScore renderGame newActor levels keyPress 2
      mkGame _ = return (pure closeAction,pure True)

  keys <- memo (keySet1 <$> keyPress)
  (output,_) <- switcher . flip fmap highScore $ \score -> do
    (menu,pick) <- displayMenu (renderMenu score) ["ONE PLAYER GAME","TWO PLAYER GAME","QUIT"] keys
    picked <- firstTrue (keyShoot <$> keys)
    gameSource <- generator (toMaybe <$> picked <*> (mkGame <$> pick))
    fullOutput <- menu --> gameSource
    return (fst =<< fullOutput,snd =<< fullOutput)

  return output

displayMenu renderMenu items keys = do
  let val False = 0
      val True  = 1

  up <- edge ((==Just North) . keyDir <$> keys)
  down <- edge ((==Just South) . keyDir <$> keys)
  item <- transfer2 0 (\u d i -> (i + val d - val u) `mod` length items) up down

  return ((renderMenu items <$> item,pure False),item)

playGame storeScore renderFun newActor levels keyPress numPlayers = mdo
  let startLevel level enemyCount = playLevel newActor keyPress level numPlayers lives' enemyCount
      players = take numPlayers [YellowWorrior,BlueWorrior]

      pickLevel cnt rnd = case () of
        _ | cnt == 4         -> findLevel (=="arena")
        _ | cnt < 8          -> findLevel ((=='b').head)
        _ | cnt `mod` 6 == 1 -> findLevel (=="pit")
        otherwise            -> findLevel ((=='w').head)
        where findLevel p = pickOne (filter (p.levelName) levels)
              pickOne xs = xs !! (rnd `mod` length xs)

      accumScore player state prev = max 0 $ prev +
                                     (sum . map killScore . actors) state -
                                     10 * (length . filter (==player) . shooters) state
        where killScore a = if justDied a && action a == KilledBy player then actorValue a else 0

      accumLives player state prev = max 0 $ prev + (sum . map lifeLost . actors) state
        where lifeLost a = if justDied a && actorType a == player then -1 else 0

  levelNoise <- noise
  (state,trigLevel) <- switcher (startLevel <$> (pickLevel <$> levelCount <*> levelNoise) <*> levelCount)
  trigLevel' <- delay False trigLevel
  levelCount <- transfer 1 (\win cnt -> if win then cnt+1 else cnt) trigLevel'
  scores <- memo =<< (sequence <$> forM players (\t -> transfer 0 (accumScore t) state))
  lives <- memo =<< (sequence <$> forM players (\t -> transfer numberOfLives (accumLives t) state))
  lives' <- delay (replicate numPlayers numberOfLives) lives

  gameOver <- memo (all (==0) <$> lives)

  return (liftA2 (>>)
          ((\c s -> when c (storeScore s)) <$> gameOver <*> scores)
          (renderFun <$> state <*> levelCount <*> scores <*> lives)
         ,gameOver
         )

playLevel newActor keyPress level numPlayers lives enemyCount = mdo
  let mkEnemy etype = enemy (newActor etype) (if etype == Worluk then 2 else 1) level bullets

      mkShot c plr = if c && canShoot
                     then (:[]) <$> bullet (actorType plr) (position plr) (facing plr)
                     else return []
        where canShoot = case action plr of
                Walking  -> True
                Shooting -> True
                _        -> False

      spawnEnemies oldEnemies = concatMap (spawnEnemy (length oldEnemies == 1)) oldEnemies
      spawnEnemy isLast enemy = if justDied enemy then
                                    case actorType enemy of
                                      Burwor  -> [mkEnemy Garwor <$> (position <$> head players)]
                                      Garwor  -> [mkEnemy Thorwor <$> (position <$> head players)]
                                      Thorwor -> if isLast then [mkEnemy Worluk <$> (position <$> head players)] else []
                                      _       -> []
                                else []

      (lw,lh) = levelSize level
      playerInit = take numPlayers [(YellowWorrior,keySet1,1),(BlueWorrior,keySet2,0)]

  (players,bulletSources) <- fmap unzip $ forM (zip playerInit [0..]) $ \((worType,keySet,pix),lix) -> do
    shoot <- edge (keyShoot . keySet <$> keyPress)

    (player,_respawn) <- switcher . flip fmap lives $ \ls -> do
      let (pedir,py,px) = entrances level !! pix
          playerInit = (newActor worType (V (px*fieldSize) ((lh-py-1)*fieldSize)))
                         { action = Entering pedir False
                         , facing = [East,West] !! pix
                         }
          respawn plr = ls !! lix > 1 && null (animation plr)

      if ls !! lix == 0
        then return (pure (playerInit { animation = [] }),pure False)
        else do plr <- transfer3 playerInit (movePlayer level) (keyDir . keySet <$> keyPress) shoot enemies'
                return (plr, respawn <$> plr)

    bulletSource <- generator (mkShot <$> shoot <*> player)
    return (player,bulletSource)

  newBullets <- memo (concat <$> sequence bulletSources)
  bullets <- collection newBullets (notHitAnything level <$> enemies')
  let shooters = fmap (map fst) . sequence =<< newBullets

  initialEnemies <- replicateM enemyCount (mkEnemy Burwor (V (-3*fieldSize) (-3*fieldSize)))
  spawnedEnemies <- generator (sequence <$> (sequence . spawnEnemies =<< enemies'))
  enemySource <- delay initialEnemies spawnedEnemies
  enemies <- collection enemySource (pure (not . null . animation))
  enemies' <- delay [] enemies

  return (LevelState level <$> (liftA2 (++) (sequence players) enemies) <*> bullets <*> shooters
         ,null <$> enemies
         )

bullet src pos dir = fmap ((,) src) <$> stateful pos (+3*dirVec dir)

enemy newActor moveSpeed level bullets (V playerX playerY) = mdo
  let actorInit = (newActor (fromIntegral fieldSize*startPos)) { speed = moveSpeed, animSpeed = 4 }
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

notHitAnything lev es (_,pos@(V px py)) =
  (sy <  fieldMid-bs || North `elem` legals) &&
  (sx <  fieldMid-bs || East  `elem` legals) &&
  (sy > -fieldMid+bs || South `elem` legals) &&
  (sx > -fieldMid+bs || West  `elem` legals) &&
  not hitExplosion
  where legals = legalMovesAt lev (fieldPos pos)
        (sx,sy) = fieldSub pos
        bs = 3

        hitExplosion = any hitBy es
        hitBy e = isDead e && abs (ex-px) < fieldMid && abs (ey-py) < fieldMid
          where V ex ey = position e

newDir lev rnd act = if not (canMove lev act) then Just pickedLegalDir
                     else if rnd `mod` 1000 < 992 then Nothing
                          else Just (toEnum (rnd `mod` 4))
  where legal = legalMovesAt lev (fieldPos (position act))
        pickedLegalDir = legal !! (rnd `mod` length legal)

movePlayer level mov shoot enemies plr = case action plr of
  Entering dir False -> let startMoving = isJust mov
                        in plr { action = Entering dir startMoving
                               , position = position plr + if startMoving then dirVec dir else 0
                               }
  Entering dir True -> if fieldSub (position plr) == (0,0)
                       then plr { action = Walking }
                       else plr { position = position plr + dirVec dir }
  _ -> case mov of
    Nothing  -> if action plr' /= Walking then animate plr' else plr'
    Just dir -> animate (if isAlive plr then move level dir plr' else plr')
  where plr' = case killed of
          _ | isDead plr -> plr
          Just enemy     -> plr { animation = deathAnimation (skin plr)
                                , action = KilledBy (actorType enemy)
                                , animSpeed = 6
                                }
          _ | shoot      -> plr { animation = shootAnimation (skin plr)
                                , action = Shooting
                                }
          otherwise      -> plr
        killed = find ((==getXY plr).getXY) [enemy | enemy <- enemies, isAlive enemy]
        getXY = fieldPos.position

moveEnemy level dir bs act = animate (mv act')
  where act' = case hit of
          Just (killer,_) -> act { animation = deathAnimation (skin act)
                                 , action = KilledBy killer
                                 }
          Nothing -> act
        mv = if isAlive act' then move level dir else id
        hit = find hitBy bs
        V px py = position act
        hitBy (_,(V bx by)) = abs (bx-px) < fieldMid && abs (by-py) < fieldMid

move :: Level -> Direction -> Actor -> Actor
move lev dir ent = mv ent $ fmap snd . find fst $
                   [(halfway dir || atFieldMid && dirIsLegal, dir)
                   ,(not atFieldMid && dirIsLegal, dir')
                   ,(halfway entDir || legalMove entDir, entDir)
                   ]
  where mv e Nothing  = e
        mv e (Just d) = e { position = position e + fromIntegral (speed e) * dirVec d
                          , facing = d
                          }

        dirIsLegal = legalMove dir
        legalMove d = d `elem` legalMovesAt lev (fieldPos (position ent))

        (sx,sy) = fieldSub (position ent)
        atFieldMid = 0 == if isVertical dir then sx else sy
        halfway d = 0 /= if isVertical d then sy else sx

        entDir = facing ent
        dir' = case entDir of
          North | sy > 0 -> South
          East  | sx > 0 -> West
          South | sy < 0 -> North
          West  | sx < 0 -> East
          otherwise      -> entDir

canMove :: Level -> Actor -> Bool
canMove lev ent = halfway || facing ent `elem` legalMovesAt lev (fieldPos (position ent))
  where (sx,sy) = fieldSub (position ent)
        halfway = sx /= 0 || sy /= 0

legalMovesAt lev pos = legalMoves lev ! ix pos
  where ix (x,y) = (snd (levelSize lev)-1-y,x)

animate :: Actor -> Actor
animate ent = let ent' = ent { tick = (tick ent+1) `mod` animSpeed ent }
                  adv = if tick ent' == 0 then 1 else 0
                  animation' = drop adv (animation ent') in
  case action ent' of
    Entering _ _ -> ent'
    Walking -> ent' { animation = animation' }
    Shooting -> if null animation'
                then ent' { animation = walkAnimation (skin ent')
                          , action = Walking
                          }
                else ent' { animation = animation' }
    KilledBy _ -> ent' { animation = animation' }

dirVec North = V 0 1
dirVec East  = V 1 0
dirVec South = V 0 (-1)
dirVec West  = V (-1) 0
