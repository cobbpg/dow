module Text
       ( loadCharset
       , displayString
       ) where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Graphics.Rendering.OpenGL

import Sprites

loadCharset file = do
  dat <- lines <$> readFile file
  let cs = unfoldr parseChar dat
      cpos = map (\n -> (n `mod` 8,n `div` 8)) [0..]
      cdat = array (' ','Z') (zip (map fst cs) cpos)

  allocaArray (256*256*4) $ \tex -> do
    pokeArray tex $ replicate (256*256*4) 0
    forM_ (zip cs cpos) $ \((_,cimg),(x,y)) -> do
      let mkpix :: Char -> [Word8]
          mkpix '#' = [255,255,255,255]
          mkpix _   = [255,255,255,0]
      forM_ (zip cimg [0..]) $ \(crow,ry) -> pokeArray (advancePtr tex ((x*32+1)*4+(y*32+1+ry)*256*4)) (concatMap mkpix crow)
    [tid] <- genObjectNames 1
    textureBinding Texture2D $= Just tid
    build2DMipmaps Texture2D RGBA' 256 256 (PixelData RGBA UnsignedByte tex)
    return (tid,cdat)

parseChar dat = if null dat' then Nothing else Just ((c,explodeMatrix 3 img),dat'')
  where dat' = dropWhile null dat
        c = head (head dat')
        (img,dat'') = span (not . null) (tail dat')

displayString tid cdat x y m s = do
  textureBinding Texture2D $= Just tid

  let displayChar c x = do
        let (cx,cy) = cdat ! c
            u = fromIntegral (cx*32+1)/256
            v = fromIntegral (cy*32+1)/256
        texCoord2 u (v+ch)
        vertex3 x y 0
        texCoord2 (u+cw) (v+ch)
        vertex3 (x+8*m) y 0
        texCoord2 (u+cw) v
        vertex3 (x+8*m) (y+10*m) 0
        texCoord2 u v
        vertex3 x (y+10*m) 0
      cw = 24/256
      ch = 30/256

  renderPrimitive Quads $ forM_ (zip s [0..]) $ \(c,i) ->
    displayChar c (x+i*8*m)

vertex3 x y z = vertex $ Vertex3 x y (z :: GLfloat)

texCoord2 x y = texCoord $ TexCoord2 x (y :: GLfloat)
