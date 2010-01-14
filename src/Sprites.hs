module Sprites
       ( parseSprites
       , explodeMatrix
       ) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad
import Data.Array
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Graphics.Rendering.OpenGL

import GraphUtils

parseSprites :: [String] -> IO [(String, [TextureObject])]
parseSprites dat = do
  let (coldat,sprdat) = span (\l -> not (null l) && (head l /= '[')) dat
      colmap :: Array Char [Word8]
      colmap = listArray (' ','~') (repeat [0,0,0,0]) // map mkcol coldat
      mkcol (c:dat) = (c,[r,g,b,a])
        where [(r,dat')] = reads dat
              [(g,dat'')] = reads dat'
              [(b,dat''')] = reads dat''
              [(a,dat'''')] = reads dat'''

  flip fix sprdat $ \addGroup dat ->
    createSpriteGroup colmap dat >>= \res -> case res of
      Just (name,g,dat') -> do
        gs <- addGroup dat'
        return ((name,g):gs)
      Nothing -> return []

createSpriteGroup colmap dat = do
  let dat' = dropWhile null dat
      gname = takeWhile (/=']') (tail (head dat'))

  if null dat'
    then return Nothing
    else do
    (sps,dat'') <- flip fix (tail dat',[]) $ \addSprites (dat,tids) ->
      createSprite colmap dat >>= \res -> case res of
        Just (tid,dat') -> do
          addSprites (dat',tid:tids)
        Nothing -> return (tids,dat)
    return (Just (gname,reverse sps,dat''))

createSprite colmap dat = do
  let dat' = dropWhile null dat

  if null dat' || head (head dat') /= '"'
    then return Nothing
    else do
    let (spdat,dat'') = span (\l -> not (null l) && (head l == '"')) dat'
        spdat' = explodeMatrix 3 (map (takeWhile (/='"') . tail) spdat)
        spw = length (head spdat')
        sph = length spdat'

    tid <- createTexture spw sph $ flip pokeArray [comp | lin <- spdat', pix <- lin, comp <- colmap ! pix]
    return (Just (tid,dat''))
