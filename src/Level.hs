module Level
       ( Level(..)
       , Direction(..)
       , parseLevel
       , turnRight
       , turnBack
       , turnLeft
       , isVertical
       ) where

import Control.Monad.Error
import Data.Array
import Data.List

data Level = Level
             { levelName :: String
             , levelSize :: (Int,Int)
             , legalMoves :: Array (Int,Int) [Direction]
             , doors :: [(Direction,Int)]
             , entrances :: [(Direction,Int,Int)]
             } deriving Show

data Direction = North | East | South | West
               deriving (Eq, Enum, Show)

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

  when (null lines' || length mazeLines < 3) $ fail "Nothing to parse"
  when (head nameLine /= '"') $ fail "Missing level name"

  return (Level { levelName = takeWhile (/= '"') (tail nameLine)
                , levelSize = (width,height)
                , legalMoves = listArray ((0,0),(height-1,width-1)) (makeMoves mazeLines)
                , doors = "<>^v" `onEdgeOf` mazeLines
                , entrances = map mkEntrance ("*" `onEdgeOf` mazeLines)
                }, rest)

chars `onEdgeOf` lines =
  [(d,i `div` 2) |
   (d,f) <- [(North,head),(South,last),(West,map head),(East,map last)],
   i <- findIndices (`elem` chars) (f lines)]

makeMoves lines = concat . odds $ zipWith3 f lines (tail lines) (tail (tail lines))
  where f n c s = odds $ zipWith4 f' (tail n) (tail s) c (tail (tail c))
        f' n s w e = (if n == ' ' then [North] else []) ++
                     (if s == ' ' then [South] else []) ++
                     (if w == ' ' then [West] else []) ++
                     (if e == ' ' then [East] else [])

odds (x:_:xs) = x : odds xs
odds xs = xs
