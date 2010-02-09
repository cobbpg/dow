module Vector where

data Vec = V !Int !Int deriving (Show, Eq)

instance Num Vec where
  V x1 y1 + V x2 y2 = V (x1+x2) (y1+y2)
  V x1 y1 - V x2 y2 = V (x1-x2) (y1-y2)
  V x1 y1 * V x2 y2 = V (x1*x2) (y1*y2)
  abs (V x y) = V (abs x) (abs y)
  signum (V x y) = V (signum x) (signum y)
  fromInteger x = V (fromInteger x) (fromInteger x)
