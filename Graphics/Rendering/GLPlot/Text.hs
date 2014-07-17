module Graphics.Rendering.GLPlot.Text
  ( renderText
  , drawTexture
  , renderToTexture
  , renderLegend
  ) where

import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.Cairo (Surface)
import qualified Graphics.Rendering.Pango as P
import qualified Data.ByteString.Unsafe as BSU
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU.Matrix
import Control.Lens
import Data.Traversable (forM)
import Data.Foldable (forM_)
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

-- | Draw a texture without scaling at the GL modelview coordinates given
drawTexture :: (GLdouble, GLdouble) -> TextureObject -> IO ()
drawTexture (x,y) t = do
    textureBinding Texture2D $= Just t
    activeTexture $= TextureUnit 0
    TextureSize2D w h <- get $ textureSize2D Texture2D 0
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texture Texture2D $= Enabled

    logicOp $= Nothing
    blend $= Enabled
    blendEquation $= FuncAdd
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    matrixMode $= Modelview 0
    mv <- get $ matrix (Just $ Modelview 0)
    unsafePreservingMatrix $ do
        loadIdentity
        (_, Size wx wy) <- get viewport
        scale (1/realToFrac wx) (1/realToFrac wy) (1 :: GLdouble)
        let V4 x0 y0 _ _ = fmap realToFrac
                         $ V4 0.25 0.25 0 0 ^+^ (mv ^. from m44GLmatrix) !* V4 x y 0 0
            (x1, y1) = (x0 + 2*realToFrac w, y0 + 2*realToFrac h)
        renderPrimitive Quads $ do
            texCoord' (TexCoord2 0   0 )  >>  vertex' (Vertex2 x0 y0)
            texCoord' (TexCoord2 1   0 )  >>  vertex' (Vertex2 x1 y0)
            texCoord' (TexCoord2 1 (-1))  >>  vertex' (Vertex2 x1 y1)
            texCoord' (TexCoord2 0 (-1))  >>  vertex' (Vertex2 x0 y1)
    texture Texture2D $= Disabled
    textureBinding Texture2D $= Nothing
    blend $= Disabled
  where
    vertex' = vertex :: Vertex2 GLfloat -> IO ()
    texCoord' = texCoord :: TexCoord2 GLint -> IO ()

renderLegend :: P.FontDescription -> [(Color4 Double, String)] -> IO TextureObject
renderLegend font [] = error "legend with no entries"
renderLegend font entries = do
    pango <- P.cairoCreateContext Nothing
    layouts <- forM entries $ \(_, s)->do
        layout <- P.layoutText pango s
        P.layoutSetFontDescription layout (Just font)
        return layout
    sizes <- forM layouts $ \layout->do
        (_, P.Rectangle _ _ w h) <- P.layoutGetPixelExtents layout
        return (w,h)
    let lineSpacing = 1.2
        h = maximum (map (\(_,h)->lineSpacing * realToFrac h) sizes)
        w = maximum (map (\(w,_)->w) sizes)
        colors = map fst entries

    let pad = V2 10 10
        size = V2 w (length entries * ceiling h) ^+^ 2 *^ pad
        sizeR = fmap realToFrac size
    renderToTexture (size ^. _x) (size ^. _y) $ do
        C.setOperator C.OperatorSource
        C.setSourceRGBA 0 0 0 0
        C.paint

        C.setSourceRGBA 0 0 0 0.7
        roundedRect (V2 0 0) sizeR 10
        C.fill

        forM_ (zip3 [0..] colors layouts) $ \(n,color,layout)->do
            let Color4 r g b a = fmap (round . (*0xffff)) color
            C.moveTo (realToFrac $ pad ^. _x) (n*h + realToFrac (pad ^. _y))
            P.setSourceColor $ P.Color r g b
            P.showLayout layout

roundedRect :: V2 Double -> V2 Double -> Double -> C.Render ()
roundedRect (V2 x0 y0) (V2 x1 y1) r = do
    C.newPath
    corner (x0+r)  (y0+r) (2*pi/2)  (3*pi/2)
    corner (x1-r)  (y0+r) (3*pi/2)  (4*pi/2)
    corner (x1-r)  (y1-r) (4*pi/2)  (5*pi/2)
    corner (x0+r)  (y1-r) (5*pi/2)  (6*pi/2)
    C.closePath
  where
    corner x y angle1 angle2 = C.arc x y r angle1 angle2
    corner' x y _ _ = C.lineTo x y
