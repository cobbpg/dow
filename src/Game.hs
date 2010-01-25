module Game where

import Data.Array
import Data.List
import Graphics.Rendering.OpenGL hiding (Level,position)

import Actor
import Level
import Sprites

data LevelState = LevelState
                  { level :: Level
                  , actors :: [Actor]
                  , bullets :: [Vec]
                  }

fieldSize = 16

fieldMid = fieldSize `div` 2

fieldPos (V x y) = ((x+fieldMid) `div` fieldSize, (y+fieldMid) `div` fieldSize)

fieldSub (V x y) = (mkSub x, mkSub y)
  where mkSub c = let s = c `mod` fieldSize in if s >= fieldMid then s-fieldSize else s

move :: Level -> Direction -> Actor -> Actor
move lev dir ent = mv ent $ fmap snd . find fst $
                   [(halfway dir || atFieldMid && dirIsLegal, dir)
                   ,(not atFieldMid && dirIsLegal, dir')
                   ,(halfway entDir || legalMove entDir, entDir)
                   ]
  where mv e Nothing  = e
        mv e (Just d) = e { position = position e + dirVec d
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
animate ent = let ent' = ent { tick = (tick ent+1) `mod` speed ent }
                  adv = if tick ent' == 0 then 1 else 0
                  animation' = drop adv (animation ent') in
  case action ent' of
    Walking -> ent' { animation = animation' }
    Shooting -> if null animation'
                then ent' { animation = walkAnimation (skin ent')
                          , action = Walking
                          }
                else ent' { animation = animation' }
    Dying -> ent' { animation = animation' }

dirVec North = V 0 1
dirVec East  = V 1 0
dirVec South = V 0 (-1)
dirVec West  = V (-1) 0
