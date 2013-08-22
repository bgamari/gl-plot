{-# LANGUAGE TemplateHaskell #-}

module GLPlot ( newPlot
              , Plot
              , updateCurves
              , Curve(Curve), cColor, cPoints
                -- * Convenient re-exports
              , mainLoop
              ) where

import Data.Foldable
import Control.Lens
import Linear

import Foreign.ForeignPtr.Safe
import qualified Data.Vector.Storable as V
import Graphics.UI.GLUT as GLUT
import Control.Concurrent.STM

data Curve = Curve { _cColor   :: !(Color4 GLfloat)
                   , _cPoints  :: !(V.Vector (V2 GLfloat))
                   }
makeLenses ''Curve

data Plot = Plot { _pWindow    :: !Window
                 , _pCurves   :: !(TVar [Curve])
                 }
makeLenses ''Plot

-- | GLUT must be initialized before this is called
newPlot :: String -> IO Plot
newPlot title = do
    window <- GLUT.createWindow title
    curves <- newTVarIO []
    let plot = Plot window curves
    displayCallback $= display plot
    return plot

updateCurves :: Plot -> [Curve] -> IO ()
updateCurves plot curves = do
    atomically $ writeTVar (plot^.pCurves) curves
    postRedisplay (Just $ plot ^. pWindow)

display :: Plot -> IO ()
display plot = do
    clear [ColorBuffer]
    curves <- atomically $ readTVar (plot^.pCurves)
    forM_ curves $ \c->do
        --currentColor $=
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
