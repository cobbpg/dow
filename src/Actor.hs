module Actor where

import Data.Array
import Data.List
import Data.Maybe
import Graphics.Rendering.OpenGL hiding (position)

import Level
import Sprites

data Skin = Skin
            { walkAnimation :: [TextureObject]
            , shootAnimation :: [TextureObject]
            , deathAnimation :: [TextureObject]
            }

data ActorType = BlueWorrior | YellowWorrior | Burwor | Garwor | Thorwor | Worluk | Wizard
               deriving (Eq, Ord, Enum, Ix, Show)

data Action = Walking | Shooting | Dying

data Actor = Actor
             { position :: Vec
             , facing :: Direction
             , action :: Action
             , animation :: [TextureObject]
             , skin :: Skin
             , speed :: Int
             , tick :: Int
             }

data Vec = V !Int !Int deriving (Show, Eq)

instance Num Vec where
  V x1 y1 + V x2 y2 = V (x1+x2) (y1+y2)
  V x1 y1 - V x2 y2 = V (x1-x2) (y1-y2)
  (*) = error "No (*) for vectors!"
  abs = error "No abs for vectors!"
  signum = error "No signum for vectors!"
  fromInteger = error "No fromInteger for vectors!"

mkActor skins atype pos =
  Actor { position = pos
        , facing = East
        , action = Walking
        , animation = walkAnimation (skins ! atype)
        , skin = skins ! atype
        , speed = 2
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
