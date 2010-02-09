module Level where

import Control.Monad.Error
import Data.Array
import Data.List

import Vector

data Level = Level
             { levelName :: String
             , levelSize :: (Int,Int)
             , legalMoves :: Array (Int,Int) [Direction]
             , doors :: [(Direction,Int)]
             , entrances :: [(Direction,Int,Int)]
             } deriving Show

data Direction = North | East | South | West
               deriving (Eq, Enum, Show)

fieldSize :: Int
fieldSize = 16

fieldMid :: Int
fieldMid = fieldSize `div` 2

fieldPos :: Vec -> (Int,Int)
fieldPos (V x y) = ((x+fieldMid) `div` fieldSize, (y+fieldMid) `div` fieldSize)

fieldSub :: Vec -> (Int,Int)
fieldSub (V x y) = (mkSub x, mkSub y)
  where mkSub c = let s = c `mod` fieldSize in if s >= fieldMid then s-fieldSize else s

dirFromInt :: Int -> Direction
dirFromInt = toEnum . (`mod` 4)

turnRight :: Direction -> Direction
turnRight = dirFromInt . (+1) . fromEnum

turnBack :: Direction -> Direction
turnBack = dirFromInt . (+2) . fromEnum

turnLeft :: Direction -> Direction
turnLeft = dirFromInt . (+3) . fromEnum

isVertical :: Direction -> Bool
isVertical North = True
isVertical South = True
isVertical _     = False

parseLevel :: [String] -> Either String (Level,[String])
parseLevel lines = do
  let lines' = dropWhile null lines
      nameLine = head lines'
      (mazeLines,rest) = span (not . null) (tail lines')
      width = length (head mazeLines) `div` 2
      height = length mazeLines `div` 2

      mkEntrance (North,x) = (South,-1,x)
      mkEntrance (South,x) = (North,height,x)
      mkEntrance (East,y) = (West,y,width)
      mkEntrance (West,y) = (East,y,-1)

      makeMoves lines = concat . odds $ zipWith3 f lines (tail lines) (tail (tail lines))
        where f n c s = odds $ zipWith4 f' (tail n) (tail s) c (tail (tail c))
              f' n s w e = (if n == ' ' then [North] else []) ++
                           (if s == ' ' then [South] else []) ++
                           (if w == ' ' then [West] else []) ++
                           (if e == ' ' then [East] else [])

      odds (x:_:xs) = x : odds xs
      odds xs = xs

      chars `onEdgeOf` lines =
        [(d,i `div` 2) |
         (d,f) <- [(North,head),(South,last),(West,map head),(East,map last)],
         i <- findIndices (`elem` chars) (f lines)]

  when (null lines' || length mazeLines < 3) $ fail "Nothing to parse"
  when (head nameLine /= '"') $ fail "Missing level name"

  return (Level { levelName = takeWhile (/= '"') (tail nameLine)
                , levelSize = (width,height)
                , legalMoves = listArray ((0,0),(height-1,width-1)) (makeMoves mazeLines)
                , doors = "<>^v" `onEdgeOf` mazeLines
                , entrances = map mkEntrance ("*" `onEdgeOf` mazeLines)
                }, rest)
