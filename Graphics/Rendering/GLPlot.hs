{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.GLPlot ( newPlot
                                 , Plot
                                 , updateCurves
                                 , setLimits
                                 , Curve(Curve), cColor, cPoints
                                 , Rect(..)
                                   -- * Convenient re-exports
                                 , mainLoop
                                 ) where

import Data.Foldable
import Control.Lens
import Control.Monad (when)
import Linear

import Foreign.ForeignPtr.Safe
import qualified Data.Vector.Storable as V
import Graphics.UI.GLUT as GLUT hiding (Rect)
import Control.Concurrent.STM

maxUpdateRate = 30  -- frames per second
       
data Curve = Curve { _cColor   :: !(Color4 GLfloat)
                   , _cPoints  :: !(V.Vector (V2 GLfloat))
                   }
makeLenses ''Curve

data Rect a = Rect (V2 a) (V2 a)           

data Plot = Plot { _pWindow       :: !Window
                 , _pCurves       :: !(TVar [Curve])
                 , _pLimits       :: !(TVar (Rect GLdouble))
                 , _pNeedsRedraw  :: !(TVar Bool)
                 , _pTimerRunning :: !(TVar Bool)
                 }
makeLenses ''Plot

-- | GLUT must be initialized before this is called.
-- Be sure to invoke the GLUT @mainLoop@.
newPlot :: String -> IO Plot
newPlot title = do
    window <- GLUT.createWindow title
    curves <- newTVarIO []
    needsRedraw <- newTVarIO False
    timerRunning <- newTVarIO False
    limits <- newTVarIO $ Rect (V2 0 0) (V2 1 1)
    let plot = Plot window curves limits needsRedraw timerRunning
    displayCallback $= display plot
    return plot

setLimits :: Plot -> Rect GLdouble -> IO ()
setLimits plot = scheduleUpdate plot . writeTVar (plot ^. pLimits)
          
startTimer :: Plot -> IO ()
startTimer plot = do
    running <- atomically $ swapTVar (plot ^. pTimerRunning) True
    when (not running) setTimer
  where setTimer = do
            addTimerCallback (1000 `div` maxUpdateRate) $ do
                needed <- atomically $ swapTVar (plot ^. pNeedsRedraw) False
                if needed
                  then postRedisplay (Just $ plot ^. pWindow) >> setTimer
                  else atomically $ writeTVar (plot ^. pTimerRunning) False

updateCurves :: Plot -> [Curve] -> IO ()
updateCurves plot = scheduleUpdate plot . writeTVar (plot ^. pCurves)
    
scheduleUpdate :: Plot -> STM () -> IO ()
scheduleUpdate plot update = do
    timerRunning <- atomically $ do update
                                    writeTVar (plot ^. pNeedsRedraw) True
                                    readTVar (plot ^. pTimerRunning)
    when (not timerRunning) $ startTimer plot >> postRedisplay (Just $ plot ^. pWindow)
    
display :: Plot -> IO ()
display plot = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    Rect a b <- atomically $ readTVar (plot ^. pLimits)
    matrixMode $= Projection
    loadIdentity
    ortho2D (a^._x) (b^._x) (a^._y) (b^._y)
    curves <- atomically $ readTVar (plot^.pCurves)
    forM_ curves $ \c->do
        currentColor $= c^.cColor
        drawVector $ c^.cPoints
    flush

drawVector' :: V.Vector (V2 GLfloat) -> IO ()
drawVector' = renderPrimitive Points . V.mapM_ (\(V2 x y)->vertex $ Vertex2 x y)

drawVector :: V.Vector (V2 GLfloat) -> IO ()
drawVector v =
    let (fptr, offset, length) = V.unsafeToForeignPtr v
        ptrSize = toEnum $ 4 * 2 * V.length v
    in withForeignPtr fptr $ \ptr->do
        (array:_) <- genObjectNames 1
        bindBuffer ElementArrayBuffer $= Just array
        bufferData ArrayBuffer $= (ptrSize, ptr, StaticDraw)
        clientState VertexArray $= Enabled
        arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 8 ptr
        drawArrays Points 0 (fromIntegral $ V.length v)
        bindBuffer ArrayBuffer $= Nothing
