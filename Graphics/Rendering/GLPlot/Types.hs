{-# LANGUAGE RankNTypes #-}

module Graphics.Rendering.GLPlot.Types where

import Linear
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL hiding (Points, Lines, Rect)
import qualified Data.Vector.Storable as V
import Control.Concurrent.STM

-- | An action invoked to get the points for a curve.
-- When this is called, the user has the option to pass
-- @Nothing@ to the given action if the curve's points
-- have not changed. This is structured this way
-- to allow the user to ensure that the points are
-- stable in memory throughout the copy operation.
data GetPoints = GetPoints (forall a. (Maybe (V.Vector (V2 GLfloat)) -> IO a) -> IO a)

data Curve = Curve { _cParams :: !CurveParams
                   , _cGetPoints :: GetPoints
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

data Plot = Plot { _pWindow       :: !Window
                 , _pCurves       :: !(TVar [Curve])
                 , _pLimits       :: !(TVar (Rect GLdouble))
                 , _pNeedsRedraw  :: !(TVar Bool)
                 , _pProgram      :: Program
                 }
