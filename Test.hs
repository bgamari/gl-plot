import Control.Monad (when)
import Control.Concurrent (threadDelay, forkIO)
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (GLfloat, Color4(..))
import Graphics.Rendering.GLPlot
import Control.Lens
import Linear

fpsLimit = 60

main = do
    GLFW.setErrorCallback $ Just $ \err s->do error s
    result <- GLFW.init
    when (not result) $ error "Failed to initialize GLFW"

    plot <- newPlot "Hello World!"
    setLimits plot $ Rect (V2 (-2) (-2)) (V2 2 2)
    let update t = do
        updateCurves plot $
            let d = plotData t
            in [   cPoints .~ d
                 $ cColor  .~ (Color4 0.8 0.6 0.4 0)
                 $ cStyle  .~ Lines
                 $ defaultCurve
               ]
        threadDelay $ 1000000 `div` fpsLimit
        update $ t + 1e-5
    forkIO $ update 2
    mainLoop [plot]

plotData :: GLfloat -> V.Vector (V2 GLfloat)
plotData t =
    V.map (\i->V2 (cos i) (sin $ sin $ i * sqrt t)) $
          (V.enumFromThenTo 0 0.01 200)
