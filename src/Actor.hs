module Actor where

import Data.Array
import Data.List
import Data.Maybe
import Graphics.Rendering.OpenGL hiding (position)

import Level
import Sprites
import Vector

data Skin = Skin
            { walkAnimation :: [TextureObject]
            , shootAnimation :: [TextureObject]
            , deathAnimation :: [TextureObject]
            }

data ActorType = BlueWorrior | YellowWorrior | Burwor | Garwor | Thorwor | Worluk | Wizard
               deriving (Eq, Ord, Enum, Ix, Show)

data Action = Entering Direction Bool | Walking | Shooting | KilledBy ActorType deriving Eq

data Actor = Actor
             { actorType :: ActorType
             , position :: Vec
             , facing :: Direction
             , action :: Action
             , animation :: [TextureObject]
             , skin :: Skin
             , speed :: Int
             , animSpeed :: Int
             , tick :: Int
             }

mkActor :: Array ActorType Skin -> ActorType -> Vec -> Actor
mkActor skins atype pos =
  Actor { actorType = atype
        , position = pos
        , facing = East
        , action = Walking
        , animation = walkAnimation (skins ! atype)
        , skin = skins ! atype
        , speed = 1
        , animSpeed = 2
        , tick = 0
        }

createSkins :: SpriteStore -> Array ActorType Skin
createSkins sprites = array (BlueWorrior,Wizard)
  [(BlueWorrior,
    Skin { walkAnimation = cycle (anim "blue-worrior-walk")
         , shootAnimation = anim "blue-worrior-shoot"
         , deathAnimation = concat (replicate 5 (anim "worrior-flash")) ++
                            anim "blue-worrior-death" ++ anim "common-death"
         }
   )
  ,(YellowWorrior,
    Skin { walkAnimation = cycle (anim "yellow-worrior-walk")
         , shootAnimation = anim "yellow-worrior-shoot"
         , deathAnimation = concat (replicate 5 (anim "worrior-flash")) ++
                            anim "yellow-worrior-death" ++ anim "common-death"
         }
   )
  ,(Burwor,
    Skin { walkAnimation = cycle (anim "burwor-walk")
         , shootAnimation = anim "burwor-shoot"
         , deathAnimation = anim "explosion" ++ anim "common-death"
         }
   )
  ,(Garwor,
    Skin { walkAnimation = cycle (anim "garwor-walk")
         , shootAnimation = anim "garwor-shoot"
         , deathAnimation = anim "explosion" ++ anim "common-death"
         }
   )
  ,(Thorwor,
    Skin { walkAnimation = cycle (anim "thorwor-walk")
         , shootAnimation = anim "thorwor-shoot"
         , deathAnimation = anim "explosion" ++ anim "common-death"
         }
   )
  ,(Worluk,
    Skin { walkAnimation = cycle (anim "worluk-walk")
         , shootAnimation = []
         , deathAnimation = anim "explosion" ++ anim "common-death"
         }
   )
  ,(Wizard,
    Skin { walkAnimation = cycle (anim "wizard-walk")
         , shootAnimation = anim "wizard-shoot"
         , deathAnimation = []
         }
   )]
  where anim name = maybe (error ("Cannot find animation " ++ name)) snd
                    (find ((==name).fst) sprites)

isDead :: Actor -> Bool
isDead a = case action a of
  KilledBy _ -> True
  _          -> False

isAlive :: Actor -> Bool
isAlive = not . isDead

justDied :: Actor -> Bool
justDied Actor { action = KilledBy _, animation = [_], tick = 0 } = True
justDied _ = False

actorValue :: Actor -> Int
actorValue a = case actorType a of
  Burwor        -> 100
  Garwor        -> 200
  Thorwor       -> 500
  Worluk        -> 1000
  Wizard        -> 2500
  BlueWorrior   -> 1000
  YellowWorrior -> 1000
