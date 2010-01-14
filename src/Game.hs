module Game where

import Data.Array
import Graphics.Rendering.OpenGL hiding (Level,position)

import Actor
import Level
import Sprites

fieldSize = 16

fieldMid = fieldSize `div` 2

fieldPos (V x y) = ((x+fieldMid) `div` fieldSize, (y+fieldMid) `div` fieldSize)

fieldSub (V x y) = (mkSub x, mkSub y)
  where mkSub c = let s = c `mod` fieldSize in if s >= fieldMid then s-fieldSize else s

move :: Level -> Direction -> Actor -> Actor
move lev dir ent = if halfway dir || atFieldMid && legal
                   then ent { position = position ent + dirVec dir
                            , facing = dir
                            }
                   else if not atFieldMid && legal
                        then ent { position = position ent + dirVec dir'
                                 , facing = dir'
                                 }
                        else if halfway entDir || legalMove entDir
                             then ent { position = position ent + dirVec entDir }
                             else ent
  where legal = legalMove dir
        legalMove d = d `elem` (legalMoves lev ! ix (fieldPos (position ent)))
        ix (x,y) = (snd (levelSize lev)-1-y,x)
        entDir = facing ent
        (sx,sy) = fieldSub (position ent)
        dir' = case entDir of
          North | sy > 0 -> South
          East  | sx > 0 -> West
          South | sy < 0 -> North
          West  | sx < 0 -> East
          otherwise      -> entDir
        atFieldMid = case dir of
          North -> sx == 0
          East  -> sy == 0
          South -> sx == 0
          West  -> sy == 0
        halfway d = case d of
          North -> sy /= 0
          East  -> sx /= 0
          South -> sy /= 0
          West  -> sx /= 0

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
