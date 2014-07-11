module Graphics.Rendering.GLPlot.Shaders where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Graphics.Rendering.OpenGL.GL
       
vertexShader = BS.pack $ unlines
    [ "#version 130"
    , "in  vec3 in_Position;"
    , "void main() "
    , "{"
    , "    gl_Position = vec4(in_Position.x, in_Position.y, in_Position.z, 1.0);"
    , "}"
    ]
       
fragmentShader = BS.pack $ unlines
    [ "#version 130"
    , "precision highp float;"
    , "uniform vec4 color;"
    , "out vec4 fragColor;"
    , "void main()"
    , "{"
    , "    fragColor = color;"
    , "}"
    ]

newShader :: ShaderType -> BS.ByteString -> EitherT String IO Shader
newShader ty src = do
    shader <- liftIO $ createShader ty
    liftIO $ shaderSourceBS shader $= src
    liftIO $ compileShader shader
    status <- liftIO $ get $ compileStatus shader
    if status
      then right shader
      else liftIO (get $ shaderInfoLog shader) >>= left

buildProgram :: EitherT String IO Program
buildProgram = do
    prg <- liftIO createProgram
    vertex <- newShader VertexShader vertexShader
    fragment <- newShader FragmentShader fragmentShader
    liftIO $ attachedShaders prg $= [vertex, fragment]
    liftIO $ attribLocation prg "in_Position" $= AttribLocation 0
    liftIO $ linkProgram prg
    return prg
