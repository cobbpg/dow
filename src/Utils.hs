{-# LANGUAGE RecursiveDo #-}

module Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import Data.Traversable hiding (sequence)
import FRP.Elerea.Simple

infix 2 -->

edge s = do
  s' <- delay False s
  return $ s' >>= \x -> if x then return False else s

x0 --> s = transfer x0 store s
    where store Nothing  x = x
          store (Just x) _ = x

collection source isAlive = mdo
  sig <- delay [] (map snd <$> collWithVals')
  coll <- memo (liftA2 (++) source sig)
  let collWithVals = zip <$> (sequence =<< coll) <*> coll
  collWithVals' <- memo (filter <$> ((.fst) <$> isAlive) <*> collWithVals)
  return $ map fst <$> collWithVals'

switcher gen = mdo
  trig <- memo (snd =<< pw)
  trig' <- delay True trig
  ss <- generator (toMaybe <$> trig' <*> gen)
  pw <- undefined --> ss
  return (fst =<< pw,trig)

toMaybe b s = if b then Just <$> s else pure Nothing
