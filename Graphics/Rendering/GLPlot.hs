module Graphics.Rendering.GLPlot ( newPlot
                                 , Plot
                                 , updateCurves
                                 , setLimits
                                 , Style(..)
                                 , Curve, cColor, cPoints, cStyle
                                 , defaultCurve
                                 , Rect(..)
                                   -- * Legend
                                 , setLegend
                                   -- * A simple main loop
                                 , mainLoop
                                 ) where

import Data.Foldable
import Control.Lens
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (when, forever, void, liftM, forM)
import Linear

import Foreign.ForeignPtr.Safe
import qualified Data.Vector.Storable as V
import           Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL.GL hiding (Points, Lines, Rect(..))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.Rendering.Pango as P
import Control.Concurrent
import Control.Concurrent.STM

import Graphics.Rendering.GLPlot.Types
import Graphics.Rendering.GLPlot.Text

maxUpdateRate = 30  -- frames per second

-- | GLUT must be initialized before this is called.
-- Be sure to invoke the GLUT @mainLoop@.
newPlot :: String -> IO Plot
newPlot title = do
    window <- fromMaybe (error "GLPlot: Failed to create window")
              `liftM` GLFW.createWindow 400 300 title Nothing Nothing
    GLFW.makeContextCurrent $ Just window
    setFramebufferSizeCallback window $ Just $ \_ w h->do
        GLFW.makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    viewport $= (Position 0 0, Size 400 300)

    curves <- newTVarIO []
    needsRedraw <- newTVarIO False
    limits <- newTVarIO $ Rect (V2 0 0) (V2 1 1)
    pointBuffer <- genObjectName
    getLegend <- newTVarIO $ return Nothing
    let plot = Plot { _pWindow       = window
                    , _pPointBuffer  = pointBuffer
                    , _pCurves       = curves
                    , _pLimits       = limits
                    , _pNeedsRedraw  = needsRedraw
                    , _pGetLegend    = getLegend
                    }
    display plot
    return plot

mainLoop :: [Plot] -> IO ()
mainLoop [] = return ()
mainLoop plots = do
    GLFW.pollEvents
    threadDelay $ 1000000 `div` maxUpdateRate
    plots' <- forM plots $ \plot->do
        let window = (plot ^. pWindow)
        redraw <- atomically $ swapTVar (plot ^. pNeedsRedraw) False
        when redraw $ do
            GLFW.makeContextCurrent $ Just window
            display plot
            legend <- atomically (readTVar (plot ^. pGetLegend)) >>= id
            case legend of
              Just texture -> drawTexture (-1,-0) texture
              Nothing -> return ()
            finish
            GLFW.swapBuffers window
        close <- windowShouldClose window
        return $ if close then Nothing else Just plot
    mainLoop $ catMaybes plots'

setLimits :: Plot -> Rect GLdouble -> IO ()
setLimits plot = scheduleUpdate plot . writeTVar (plot ^. pLimits)

updateCurves :: Plot -> [Curve] -> IO ()
updateCurves plot = scheduleUpdate plot . writeTVar (plot ^. pCurves)

scheduleUpdate :: Plot -> STM () -> IO ()
scheduleUpdate plot update =
    atomically $ update >> writeTVar (plot ^. pNeedsRedraw) True

display :: Plot -> IO ()
display plot = do
    depthFunc $= Nothing
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    Rect a b <- atomically $ readTVar (plot ^. pLimits)
    matrixMode $= Modelview 0
    loadIdentity
    GLU.ortho2D (a^._x) (b^._x) (a^._y) (b^._y)
    curves <- atomically $ readTVar (plot^.pCurves)
    forM_ curves $ \c->do
        currentColor $= c^.cColor
        drawVector plot (c^.cStyle) (c^.cPoints)

drawVector :: Plot -> Style -> V.Vector (V2 GLfloat) -> IO ()
drawVector plot style v =
    let (fptr, offset, length) = V.unsafeToForeignPtr v
        ptrSize = toEnum $ 4 * 2 * V.length v
        primMode = case style of
                Lines    -> GL.LineStrip
                Points   -> GL.Points
        array = plot ^. pPointBuffer
    in withForeignPtr fptr $ \ptr->do
        bindBuffer ElementArrayBuffer $= Just array
        bufferData ArrayBuffer $= (ptrSize, ptr, StreamDraw)
        clientState VertexArray $= Enabled
        arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 8 ptr
        drawArrays primMode 0 (fromIntegral $ V.length v)
        bindBuffer ArrayBuffer $= Nothing

setLegend :: Plot -> [(Color4 Double, String)] -> IO ()
setLegend plot entries = do
    scheduleUpdate plot $ do
        writeTVar (plot ^. pGetLegend) getLegend
  where
    getLegend = do
      font <- P.fontDescriptionNew
      P.fontDescriptionSetSize font 24
      legend <- renderLegend font entries
      atomically $ writeTVar (plot ^. pGetLegend) (return $ Just legend)
      return (Just legend)
