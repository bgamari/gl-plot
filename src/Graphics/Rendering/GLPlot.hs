module Graphics.Rendering.GLPlot
    ( -- * Main loop context
      Context
    , newContext
      -- * Plots
    , Plot
    , newPlot
    , setLimits
    , Rect(..)
      -- * Curves
    , Curve
    , newCurve
    , setPoints
    , defaultCurve
    , CurveParams, cColor, cStyle, cName
    , Style(..)
      -- * Diagnostics
    , getFrameCount
    ) where

import Data.Foldable
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import qualified Data.Text as T
import Control.Monad (when, forever, void, liftM, forM)
import Control.Monad.Trans.Except
import Control.Lens hiding (Context)
import Linear
import Linear.OpenGL

import Foreign.ForeignPtr
import Foreign.Ptr (nullPtr)
import qualified Data.Vector.Storable as V
import           Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL.GL hiding (Points, Lines, Rect(..))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GLU as GLU
import qualified GI.Pango as P
import Control.Concurrent
import Control.Concurrent.STM

import Graphics.Rendering.GLPlot.Types
import Graphics.Rendering.GLPlot.Lenses
import Graphics.Rendering.GLPlot.Shaders
import Graphics.Rendering.GLPlot.Text

maxUpdateRate = 30  -- frames per second

-- | Only one context should be created per process.
newContext :: IO Context
newContext = do
    GLFW.setErrorCallback $ Just $ \err s->fail s
    result <- GLFW.init
    when (not result) $ error "Failed to initialize GLFW"

    taskQueue <- newTQueueIO
    forkOS $ forever $ do
        task <- atomically $ readTQueue taskQueue
        task

    let ctx = Context { _ctxTasks = taskQueue }
    return ctx

-- | Schedule a task to be run in the main loop
scheduleTask :: Context -> IO () -> IO ()
scheduleTask ctx task = atomically $ writeTQueue (ctx ^. ctxTasks) task

-- | Perform the given action with the plot's GL context active
withPlotContext :: Plot -> (Window -> IO a) -> IO a
withPlotContext plot action = do
    window <- atomically $ takeTMVar $ plot ^. pWindow
    GLFW.makeContextCurrent (Just window)
    r <- action window
    GLFW.makeContextCurrent Nothing
    atomically $ putTMVar (plot ^. pWindow) window
    return r

-- | Schedule a task with the given plot's GL context active
schedulePlotTask :: Plot -> (Window -> IO ()) -> IO ()
schedulePlotTask plot action =
    scheduleTask (plot ^. pMainloop) $ withPlotContext plot action

-- | Schedule a redraw of a plot
scheduleRedraw :: Plot -> IO ()
scheduleRedraw plot = do
    scheduled <- atomically $ swapTVar (plot ^. pRedrawScheduled) True
    when (not scheduled) $ schedulePlotTask plot (redrawPlot plot)

redrawPlot :: Plot -> Window -> IO ()
redrawPlot plot window = do
    atomically $ writeTVar (plot ^. pRedrawScheduled) False
    drawPlot plot
    legend <- atomically $ readTVar (plot ^. pLegend)
    case legend of
        Just texture -> drawTexture (-0,0) texture
        Nothing -> return ()
    finish
    GLFW.swapBuffers window
    atomically $ modifyTVar (plot ^. pFrameCount) (+1)

-- | Create a new plot
newPlot :: Context -> String -> IO Plot
newPlot mainloop title = do
    -- we can apparently get away with doing this initialization
    -- outside of the mainloop
    window <- fromMaybe (error "GLPlot: Failed to create window")
              `liftM` GLFW.createWindow 400 300 title Nothing Nothing
    curves <- newTVarIO []
    redrawScheduled <- newTVarIO False
    legendPos <- newTVarIO Nothing
    legend <- newTVarIO Nothing
    limits <- newTVarIO $ Rect (V2 0 0) (V2 1 1)

    GLFW.makeContextCurrent (Just window)
    let shaderError e = error $ "Failed to build shader program: "++e
    program <- either shaderError id `fmap` runExceptT buildProgram
    GLFW.makeContextCurrent Nothing

    windowVar <- newTMVarIO window
    frameCount <- newTVarIO 0
    let plot = Plot { _pWindow       = windowVar
                    , _pCurves       = curves
                    , _pLimits       = limits
                    , _pMainloop     = mainloop
                    , _pRedrawScheduled = redrawScheduled
                    , _pLegendPos    = legendPos
                    , _pLegend       = legend
                    , _pProgram      = program
                    , _pFrameCount   = frameCount
                    }

    withPlotContext plot $ const $ do
        viewport $= (Position 0 0, Size 400 300)
        setFramebufferSizeCallback window
            $ Just $ \_ w h->schedulePlotTask plot $ const $ do
                viewport $= ( Position 0 0
                            , Size (fromIntegral w) (fromIntegral h))
                redrawPlot plot window

    -- Ensure we periodically poll GLFW for events
    forkIO $ forever $ do
        schedulePlotTask plot $ const $ do
            close <- windowShouldClose window
            when close $ destroyWindow window
            -- TODO: ensure further activity isn't allowed
            GLFW.pollEvents
        threadDelay $ 1000000 `div` 10

    scheduleRedraw plot
    return plot

-- | Get the number of frames rendered since the last call to
-- @getFrameCount@
getFrameCount :: Plot -> IO Int
getFrameCount plot = atomically $ swapTVar (plot ^. pFrameCount) 0

-- | Set the bounds of the plot area
setLimits :: Plot -> Rect GLdouble -> IO ()
setLimits plot limits = do
    atomically $ writeTVar (plot ^. pLimits) limits
    scheduleRedraw plot

-- | Create a new curve
newCurve :: Plot -> CurveParams -> IO Curve
newCurve plot params = withPlotContext plot $ const $ do
    buffer <- genObjectName
    points <- newTVarIO 0
    let s = Curve { _cParams = params
                  , _cBuffer = buffer
                  , _cPoints = points
                  , _cPlot = plot
                  }
    atomically $ modifyTVar (plot ^. pCurves) (s:)
    schedulePlotTask plot (const $ redrawPlotLegend plot)
    return s

-- | Re-generate the legend for a plot
redrawPlotLegend :: Plot -> IO ()
redrawPlotLegend plot = schedulePlotTask plot $ const $ do
    curves <- atomically $ readTVar (plot ^. pCurves)
    let curveToEntry c =
          case p ^. cName of
            Just name -> Just (fmap realToFrac $ p ^. cColor, T.pack name)
            Nothing   -> Nothing
          where p = c ^. cParams
        entries = mapMaybe curveToEntry curves
    case entries of
      [] -> return ()
      _  -> drawLegend entries
  where
    drawLegend entries = do
        font <- P.fontDescriptionNew
        P.fontDescriptionSetSize font 24
        legend <- renderLegend font entries
        oldLegend <- atomically $ do
            old <- readTVar (plot ^. pLegend)
            writeTVar (plot ^. pLegend) (Just legend)
            return old
        case oldLegend of
          Nothing -> return ()
          Just texture -> deleteObjectName texture

-- | Upload the given points buffer
uploadPoints :: Curve -> V.Vector (V2 GLfloat) -> IO ()
uploadPoints curve pts =
    let (fptr, offset, length) = V.unsafeToForeignPtr pts
        ptrSize = toEnum $ 4 * 2 * V.length pts
        array = curve ^. cBuffer
    in withForeignPtr fptr $ \ptr->do
         bindBuffer ArrayBuffer $= Just array
         bufferData ArrayBuffer $= (ptrSize, ptr, StreamDraw)
         atomically $ writeTVar (curve ^. cPoints) (V.length pts)

-- | Set the points of a curve
setPoints :: Curve -> V.Vector (V2 GLfloat) -> IO ()
setPoints curve pts = withPlotContext plot $ const $ do
    uploadPoints curve pts
    scheduleRedraw plot
  where plot = curve ^. cPlot

-- | Draw the given plot
drawPlot :: Plot -> IO ()
drawPlot plot = do
    depthFunc $= Nothing
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    Rect a b <- atomically $ readTVar (plot ^. pLimits)
    matrixMode $= Modelview 0
    loadIdentity
    GLU.ortho2D (a^._x) (b^._x) (a^._y) (b^._y)
    currentProgram $= Just (plot ^. pProgram)
    curves <- atomically $ readTVar (plot^.pCurves)
    forM_ curves drawCurve
    currentProgram $= Nothing

-- | Draw the given curve
drawCurve :: Curve -> IO ()
drawCurve c = do
    loc <- get $ uniformLocation (c ^. cPlot . pProgram) "color"
    uniform loc $= c ^. cParams . cColor

    loc <- get $ uniformLocation (c ^. cPlot . pProgram) "matrix"
    mat <- get $ matrix (Just $ Modelview 0) :: IO (GLmatrix GLfloat)
    uniform loc $= (mat ^. from m44GLmatrix)

    nPoints <- atomically $ readTVar (c ^. cPoints)
    let primMode = case c ^. cParams . cStyle of
                Lines    -> GL.LineStrip
                Points   -> GL.Points
    bindBuffer ArrayBuffer $= Just (c ^. cBuffer)
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float 8 nullPtr)
    vertexAttribArray (AttribLocation 0) $= Enabled
    drawArrays primMode 0 (fromIntegral nPoints)
    vertexAttribArray (AttribLocation 0) $= Disabled
    bindBuffer ArrayBuffer $= Nothing
