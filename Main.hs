import Control.Monad (when, forever)
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (GLfloat, Color4(..))
import Graphics.Rendering.GLPlot
import Control.Lens
import Linear

main :: IO ()
main = do
    ctx <- newContext
    plot <- newPlot ctx "Plot"
    setLimits plot $ Rect (V2 (-2) (-2)) (V2 2 2)

    let params = cColor  .~ (Color4 0.1 0.6 0.4 0)
               $ cStyle  .~ Lines
               $ defaultCurve
    curve <- newCurve plot params
    setPoints curve mempty

    points <- VM.replicate 100000 0
    forkIO $ reader points

    forever $ do
        d <- V.freeze points
        setPoints curve d

        let x0 = realToFrac $ V.minimum $ V.map (^. _x) d
            x1 = realToFrac $ V.maximum $ V.map (^. _x) d
            y0 = realToFrac $ V.minimum $ V.map (^. _y) d
            y1 = realToFrac $ V.maximum $ V.map (^. _y) d
        setLimits plot $ Rect (V2 x0 y0) (V2 x1 y1)

        threadDelay $ 1000000 `div` 30

reader :: VM.MVector VM.RealWorld (V2 GLfloat) -> IO ()
reader points = go 0
  where
    go i = do
        x:y:_ <- map read . words <$> getLine
        VM.write points (i `mod` VM.length points) (V2 x y)
        print (V2 x y)
        go (i+1)
