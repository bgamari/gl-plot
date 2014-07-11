import Control.Monad (when, forever)
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (GLfloat, Color4(..))
import Graphics.Rendering.GLPlot
import Control.Lens
import Linear

main = do
    ctx <- newContext
    plot <- newPlot ctx "Hello World!"
    setLimits plot $ Rect (V2 (-2) (-2)) (V2 2 2)
    --setLegend plot [ (Color4 1 0 0 1, "Hello World!")
    --               , (Color4 0 1 0 1, "Hello Again!")
    --               , (Color4 0 0 1 1, "Ho ho ho ho!")
    --               ]
    time <- newIORef 2
    let params = cColor  .~ (Color4 0.8 0.6 0.4 0)
               $ cStyle  .~ Lines
               $ defaultCurve
        update push = do
          t <- readIORef time
          modifyIORef time (+1e-5)
          push $ Just $ plotData t
    curve <- newCurve plot params (GetPoints update)

    done <- newIORef False
    let params2 = cColor  .~ (Color4 0.1 0.6 0.4 0)
                $ cStyle  .~ Lines
                $ defaultCurve
        update2 push = do
          d <- readIORef done
          writeIORef done True
          if d
            then push Nothing
            else push $ Just $ plotData 5
    curve <- newCurve plot params2 (GetPoints update2)
    forkIO $ runContext ctx
    forever $ do
        scheduleRedraw plot
        threadDelay $ 1000000 `div` 30

plotData :: GLfloat -> V.Vector (V2 GLfloat)
plotData t =
    V.map (\i->V2 (cos i) (sin $ sin $ i * sqrt t)) $
          (V.enumFromThenTo 0 0.01 200)
