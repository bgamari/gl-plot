import Data.IORef
import Foreign.ForeignPtr.Safe
import qualified Data.Vector.Storable as V
import Linear

import Graphics.UI.GLUT as GLUT
import Shaders

main = do
    GLUT.getArgsAndInitialize
    window <- GLUT.createWindow "Hello World!"
    tRef <- newIORef 0
    displayCallback $= display tRef
    
    let update = do modifyIORef' tRef (+0.1)
                    postRedisplay (Just window)
                    addTimerCallback 30 update
    update
    GLUT.mainLoop
    
plotData :: GLfloat -> V.Vector (V2 GLfloat)
plotData t = V.map (\t->V2 (cos t) (sin t)) (V.enumFromThenTo t (t+0.02) (t+pi))

display :: IORef GLfloat -> IO ()
display tRef = do
    clear [ ColorBuffer]
    --renderPrimitive Points $ V.mapM_ (\(V2 x y)->vertex $ Vertex2 x y) plotData
    t <- readIORef tRef
    drawVector $ plotData t
    flush

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

    
drawVector' :: V.Vector (V2 GLfloat) -> IO ()
drawVector' v = do
    let (fptr, offset, length) = V.unsafeToForeignPtr v
        ptrSize = toEnum $ 4 * 2 * V.length v
    withForeignPtr fptr $ \ptr->do
    (array:_) <- genObjectNames 1
    bindBuffer ElementArrayBuffer $= Just array
    bufferData ArrayBuffer $= (ptrSize, ptr, StaticDraw)

    --program <- loadProgram "vertex.glsl" "fragment.glsl"
    --coordLoc <- get $ uniformLocation program "coord"
    --colorLoc <- get $ uniformLocation program "color"
    --vertexAttribPointer coordLoc $= (ToFloat, VertexArrayDescriptor 2 Float 8 ptr)
