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
        updateCurves plot $
            let d = plotData t
            in [ Curve (Color4 0 0 0 0) d Lines
               , Curve (Color4 1 0 0 0) (V.map (+0.1) d) Points
               ]
        threadDelay 30000
        update $ t + 1e-5
    forkIO $ update 2
    GLUT.mainLoop

plotData :: GLfloat -> V.Vector (V2 GLfloat)
plotData t =
    V.map (\i->V2 (cos i) (sin $ sin $ i * sqrt t)) $
          (V.enumFromThenTo 0 0.01 200)
