{-# LANGUAGE RecursiveDo #-}

module Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import FRP.Elerea.Experimental.Simple

infix 2 -->

transfer2 x0 f s1 s2 = mfix $ \sig -> liftA3 f s1 s2 <$> delay x0 sig

transfer3 x0 f s1 s2 s3 = mfix $ \sig -> liftM4 f s1 s2 s3 <$> delay x0 sig

transfer4 x0 f s1 s2 s3 s4 = mfix $ \sig -> liftM5 f s1 s2 s3 s4 <$> delay x0 sig

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
  trig' <- delay (Just undefined) trig
  ss <- generator (toMaybe . isJust <$> trig' <*> gen)
  pw <- undefined --> ss
  return (fst =<< pw,trig)

toMaybe b s = if b then Just <$> s else pure Nothing
