{-# LANGUAGE RankNTypes #-}

module Graphics.Rendering.GLPlot.Types where

import Linear
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL hiding (Points, Lines, Rect)
import qualified Data.Vector.Storable as V
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM

-- | A main loop context
data Context = Context { _ctxTasks    :: !(TQueue (IO ()))
                       }

data Curve = Curve { _cParams :: !CurveParams
                   , _cBuffer :: !BufferObject
                   , _cPoints :: !(TVar Int)
                   , _cPlot   :: !Plot
                   }

data Style = Lines | Points

data CurveParams = CurveParams { _cColor   :: !(Color4 GLfloat)
                               , _cStyle   :: !Style
                               , _cName    :: !(Maybe String)
                               }

defaultCurve :: CurveParams
defaultCurve = CurveParams { _cColor  = Color4 0 0 0 0
                           , _cStyle  = Points
                           , _cName   = Nothing
                           }

data Rect a = Rect (V2 a) (V2 a)

data Plot = Plot { _pWindow       :: !(TMVar Window)
                 , _pCurves       :: !(TVar [Curve])
                 , _pLimits       :: !(TVar (Rect GLdouble))
                 , _pMainloop     :: !Context
                 -- , _pDrawLegend   :: !Bool
                 , _pLegend       :: !(TVar (Maybe TextureObject))
                 , _pProgram      :: Program
                 , _pFrameCount   :: !(TVar Int)
                 }
