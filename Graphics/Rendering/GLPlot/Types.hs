{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.GLPlot.Types where

import Control.Lens
import Linear
import Graphics.UI.GLUT as GLUT hiding (Rect, Points, Lines)
import qualified Data.Vector.Storable as V
import Control.Concurrent.STM

data Style = Lines | Points

data Curve = Curve { _cColor   :: !(Color4 GLfloat)
                   , _cPoints  :: !(V.Vector (V2 GLfloat))
                   , _cStyle   :: !Style
                   , _cName    :: !(Maybe String)
                   }
makeLenses ''Curve

defaultCurve :: Curve
defaultCurve = Curve { _cColor  = Color4 0 0 0 0
                     , _cPoints = V.empty
                     , _cStyle  = Points
                     , _cName   = Nothing
                     }

data Rect a = Rect (V2 a) (V2 a)

data Plot = Plot { _pWindow       :: !Window
                 , _pPointBuffer  :: !BufferObject
                 , _pCurves       :: !(TVar [Curve])
                 , _pLimits       :: !(TVar (Rect GLdouble))
                 , _pNeedsRedraw  :: !(TVar Bool)
                 , _pTimerRunning :: !(TVar Bool)
                 }
makeLenses ''Plot
