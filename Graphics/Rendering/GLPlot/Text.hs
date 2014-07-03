module Graphics.Rendering.GLPlot.Text
  ( renderText
  , drawTexture
  , renderToTexture
  ) where

import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.Cairo (Surface)
import qualified Graphics.Rendering.Pango as P
import qualified Data.ByteString.Unsafe as BSU
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU.Matrix
import Control.Lens
import Linear
import Linear.OpenGL

renderText :: String -> IO TextureObject
renderText text = do
    font <- P.fontDescriptionNew
    P.fontDescriptionSetSize font 24

    pango <- P.cairoCreateContext Nothing
    layout <- P.layoutText pango text
    P.layoutSetFontDescription layout (Just font)
    (_, P.Rectangle _ _ w h) <- P.layoutGetPixelExtents layout

    renderToTexture w h $ do
        C.setSourceRGBA 0 0 0 0
        C.paint
        C.setSourceRGBA 1 1 1 1
        P.showLayout layout

renderToTexture :: Int -> Int -> C.Render a -> IO TextureObject
renderToTexture w h render = do
    surf <- C.createImageSurface C.FormatARGB32 w h
    C.renderWith surf render
    texture <- genObjectName
    textureBinding Texture2D $= Just texture
    d <- C.imageSurfaceGetData surf
    BSU.unsafeUseAsCString d $ \ptr->do
      let size = TextureSize2D (fromIntegral w) (fromIntegral h)
          pixelData = PixelData BGRA UnsignedByte ptr
      texImage2D Texture2D NoProxy 0 RGBA' size 0 pixelData
    return texture

drawTexture :: (GLdouble, GLdouble) -> TextureObject -> IO ()
drawTexture (x,y) t = do
    textureBinding Texture2D $= Just t
    TextureSize2D w h <- get $ textureSize2D Texture2D 0
    blend $= Enabled
    blendFunc $= (SrcColor, OneMinusSrcAlpha)
    currentColor $= Color4 0 0 0 1
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture Texture2D $= Enabled
    matrixMode $= Modelview 0
    mv <- get $ matrix (Just $ Modelview 0)
    unsafePreservingMatrix $ do
        loadIdentity
        (_, Size wx wy) <- get viewport
        scale (1/realToFrac wx) (1/realToFrac wy) (1 :: GLdouble)
        let V4 x0 y0 _ _ = fmap realToFrac $ (mv ^. from m44GLmatrix) !* V4 x y 0 0
            (x1, y1) = (x0 + 2*realToFrac w, y0 + 2*realToFrac h)

        renderPrimitive Quads $ do
            texCoord' (TexCoord2 0   0 )  >>  vertex' (Vertex2 x0 y0)
            texCoord' (TexCoord2 1   0 )  >>  vertex' (Vertex2 x1 y0)
            texCoord' (TexCoord2 1 (-1))  >>  vertex' (Vertex2 x1 y1)
            texCoord' (TexCoord2 0 (-1))  >>  vertex' (Vertex2 x0 y1)
    texture Texture2D $= Disabled
    blend $= Disabled
  where
    vertex' = vertex :: Vertex2 GLfloat -> IO ()
    texCoord' = texCoord :: TexCoord2 GLint -> IO ()
