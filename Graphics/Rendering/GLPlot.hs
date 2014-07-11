module Graphics.Rendering.GLPlot ( newPlot
                                 , Plot
                                 , setLimits
                                 , Rect(..)
                                 , Curve
                                 , newCurve
                                 , GetPoints(..)
                                 , defaultCurve
                                 , CurveParams, cColor, cStyle, cName
                                 , Style(..)
                                   -- * A simple main loop
                                 , mainLoop
                                 ) where

import Data.Foldable
import Control.Lens
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (when, forever, void, liftM, forM)
import Linear

import Foreign.ForeignPtr.Safe
import Foreign.Ptr (nullPtr)
import qualified Data.Vector.Storable as V
import Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL.GL hiding (Points, Lines, Rect(..))
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Control.Concurrent
import Control.Concurrent.STM

import Graphics.Rendering.GLPlot.Types
import Graphics.Rendering.GLPlot.Lenses

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
    let plot = Plot { _pWindow       = window
                    , _pCurves       = curves
                    , _pLimits       = limits
                    , _pNeedsRedraw  = needsRedraw
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
        --redraw <- atomically $ swapTVar (plot ^. pNeedsRedraw) False
        let redraw = True
        when redraw $ do GLFW.makeContextCurrent $ Just window
                         display plot
                         print "hi"
                         finish
                         GLFW.swapBuffers window
        close <- windowShouldClose window
        return $ if close then Nothing else Just plot
    mainLoop $ catMaybes plots'

-- | Set the bounds of the plot area
setLimits :: Plot -> Rect GLdouble -> IO ()
setLimits plot = scheduleUpdate plot . writeTVar (plot ^. pLimits)

updateCurves :: Plot -> [Curve] -> IO ()
updateCurves plot = scheduleUpdate plot . writeTVar (plot ^. pCurves)

scheduleUpdate :: Plot -> STM () -> IO ()
scheduleUpdate plot update =
    atomically $ update >> writeTVar (plot ^. pNeedsRedraw) True

newCurve :: Plot -> CurveParams -> GetPoints -> IO Curve
newCurve plot params getPoints = do
    buffer <- genObjectName
    points <- newTVarIO 0
    let s = Curve { _cParams = params
                  , _cGetPoints = getPoints
                  , _cBuffer = buffer
                  , _cPoints = points
                  , _cPlot = plot
                  }
    atomically $ modifyTVar (plot ^. pCurves) (s:)
    return s

setPoints :: Curve -> V.Vector (V2 GLfloat) -> IO ()
setPoints curve pts =
    let (fptr, offset, length) = V.unsafeToForeignPtr pts
        ptrSize = toEnum $ 4 * 2 * V.length pts
        array = curve ^. cBuffer
    in withForeignPtr fptr $ \ptr->do
         bindBuffer ElementArrayBuffer $= Just array
         bufferData ArrayBuffer $= (ptrSize, ptr, StreamDraw)
         atomically $ writeTVar (curve ^. cPoints) (V.length pts)

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
        let updatePoints Nothing = return False
            updatePoints (Just pts) = setPoints c pts >> return True
            GetPoints getPoints = c ^. cGetPoints
        updated <- getPoints updatePoints
        drawCurve c

drawCurve :: Curve -> IO ()
drawCurve c = do
    currentColor $= (c ^. cParams . cColor)
    nPoints <- atomically $ readTVar (c ^. cPoints)
    let primMode = case c ^. cParams . cStyle of
                Lines    -> GL.LineStrip
                Points   -> GL.Points
    bindBuffer ElementArrayBuffer $= Just (c ^. cBuffer)
    clientState VertexArray $= Enabled
    arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 8 nullPtr
    drawArrays primMode 0 (fromIntegral nPoints)
    clientState VertexArray $= Disabled
    bindBuffer ArrayBuffer $= Nothing
