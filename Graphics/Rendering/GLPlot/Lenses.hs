{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Rendering.GLPlot.Lenses where

import Graphics.Rendering.GLPlot.Types
import Control.Lens

makeLenses ''Plot
makeLenses ''Curve
makeLenses ''CurveParams
