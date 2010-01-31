module GraphUtils where

import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Graphics.Rendering.OpenGL

createTexture :: Int -> Int -> Bool -> (Ptr Word8 -> IO ()) -> IO TextureObject
createTexture width height looped fill = allocaArray (width*height*4) $ \tex -> do
  fill tex
  [tid] <- genObjectNames 1
  textureBinding Texture2D $= Just tid
  if looped
    then do textureWrapMode Texture2D S $= (Repeated,Repeat)
            textureWrapMode Texture2D T $= (Repeated,Repeat)
    else do textureWrapMode Texture2D S $= (Mirrored,Clamp)
            textureWrapMode Texture2D T $= (Mirrored,Clamp)
  build2DMipmaps Texture2D RGBA' (fromIntegral width) (fromIntegral height) (PixelData RGBA UnsignedByte tex)
  return tid

vertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3 x y z = vertex $ Vertex3 x y z

texCoord2 :: GLfloat -> GLfloat -> IO ()
texCoord2 x y = texCoord $ TexCoord2 x y

explodeList :: Int -> [a] -> [a]
explodeList = concatMap . replicate

explodeMatrix :: Int -> [[a]] -> [[a]]
explodeMatrix n = map (explodeList n) . explodeList n
