import Control.Concurrent (threadDelay, forkIO)
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (GLfloat, Color4(..))
import Graphics.Rendering.GLPlot
import Linear

main = do
    GLUT.getArgsAndInitialize
    plot <- newPlot "Hello World!"
    setLimits plot $ Rect (V2 (-2) (-2)) (V2 2 2)
    let update t = do
        updateCurves plot [ Curve (Color4 0 0 0 0) (plotData t) Lines
                          , Curve (Color4 1 0 0 0) (V.map (+0.1) $ plotData t) Points
                          ]
        threadDelay 30000
        update $ t + 1
    forkIO $ update 0
    GLUT.mainLoop

plotData :: GLfloat -> V.Vector (V2 GLfloat)
plotData t =
    V.map (\t->V2 (cos t) (sin $ sin $ t*sqrt 2))
          (V.enumFromThenTo t (t+0.01) (t+200))
