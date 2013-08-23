module Graphics.Rendering.GLPlot ( newPlot
                                 , Plot
                                 , updateCurves
                                 , setLimits
                                 , Style(..)
                                 , Curve, cColor, cPoints, cStyle
                                 , defaultCurve
                                 , Rect(..)
                                   -- * A simple main loop
                                 , mainLoop
                                 ) where

import Data.Foldable
import Control.Lens
import Data.Maybe (fromMaybe)
import Control.Monad (when, forever, void, liftM)
import Linear

import Foreign.ForeignPtr.Safe
import qualified Data.Vector.Storable as V
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL hiding (Rect, Points, Lines)
import Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.Rendering.OpenGL.GL.PrimitiveMode as PrimitiveMode
import Control.Concurrent
import Control.Concurrent.STM

import Graphics.Rendering.GLPlot.Types

maxUpdateRate = 30  -- frames per second

-- | GLUT must be initialized before this is called.
-- Be sure to invoke the GLUT @mainLoop@.
newPlot :: String -> IO Plot
newPlot title = do
    window <- fromMaybe (error "GLPlot: Failed to create window")
              `liftM` GLFW.createWindow 400 300 title Nothing Nothing
    GLFW.makeContextCurrent $ Just window
    setFramebufferSizeCallback window $ Just $ \_ w h->do
        print (w,h)
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    viewport $= (Position 0 0, Size 400 300)

    curves <- newTVarIO []
    needsRedraw <- newTVarIO False
    limits <- newTVarIO $ Rect (V2 0 0) (V2 1 1)
    (pointBuffer:_) <- genObjectNames 1
    let plot = Plot { _pWindow       = window
                    , _pPointBuffer  = pointBuffer
                    , _pCurves       = curves
                    , _pLimits       = limits
                    , _pNeedsRedraw  = needsRedraw
                    }
    display plot
    return plot

mainLoop :: Plot -> IO ()
mainLoop plot = do
    GLFW.pollEvents
    threadDelay 30000
    redraw <- atomically $ swapTVar (plot ^. pNeedsRedraw) False
    when redraw $ display plot
    close <- windowShouldClose (plot ^. pWindow)
    finish
    GLFW.swapBuffers (plot ^. pWindow)
    when (not close) $ mainLoop plot

setLimits :: Plot -> Rect GLdouble -> IO ()
setLimits plot = scheduleUpdate plot . writeTVar (plot ^. pLimits)

updateCurves :: Plot -> [Curve] -> IO ()
updateCurves plot = scheduleUpdate plot . writeTVar (plot ^. pCurves)

scheduleUpdate :: Plot -> STM () -> IO ()
scheduleUpdate plot update =
    atomically $ update >> writeTVar (plot ^. pNeedsRedraw) True

display :: Plot -> IO ()
display plot = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    Rect a b <- atomically $ readTVar (plot ^. pLimits)
    matrixMode $= Projection
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
                Lines    -> PrimitiveMode.LineStrip
                Points   -> PrimitiveMode.Points
        array = plot ^. pPointBuffer
    in withForeignPtr fptr $ \ptr->do
        bindBuffer ElementArrayBuffer $= Just array
        bufferData ArrayBuffer $= (ptrSize, ptr, StreamDraw)
        clientState VertexArray $= Enabled
        arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 8 ptr
        drawArrays primMode 0 (fromIntegral $ V.length v)
        bindBuffer ArrayBuffer $= Nothing
