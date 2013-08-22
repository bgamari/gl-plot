import Control.Concurrent (threadDelay, forkIO)
import qualified Data.Vector.Storable as V
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.GLPlot
import Linear

main = do
    GLUT.getArgsAndInitialize
    plot <- newPlot "Hello World!"
    setLimits plot $ Rect (V2 0 0) (V2 2 3)
    let update t = do
        updateCurves plot [ Curve (Color4 0 0 0 0) (plotData t)
                          , Curve (Color4 1 0 0 0) (V.map (+0.1) $ plotData t)
                          ]
        threadDelay 30000
        update $ t + 1
    forkIO $ update 0
    GLUT.mainLoop

plotData :: GLfloat -> V.Vector (V2 GLfloat)
plotData t = V.map (\t->V2 (cos t) (sin t)) (V.enumFromThenTo t (t+0.02) (t+pi))
