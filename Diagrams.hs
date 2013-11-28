{-# LANGUAGE TypeFamilies                  #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams (
    example
  , errChart
  ) where

import Diagrams.Prelude hiding ( render )
import qualified Diagrams.Prelude as P
import Diagrams.Coordinates ( (^&) )
import Diagrams.TwoD.Arrow

import Data.List (minimumBy, tails, (\\))
import Data.Ord (comparing)

import Control.Lens hiding ( (#), (&), moveTo )
import Graphics.Rendering.Chart hiding ( moveTo )
import Graphics.Rendering.Chart.Backend.Cairo hiding (runBackend, defaultEnv)
-- import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Default.Class


type Square = (Int, Int)

board :: [Square]
board = [ (x,y) | x <- [0..7], y <- [0..7] ]

boardSq' c = arr <> square 1 # lw 0 # fc c
  where
    arr = arrowBetween' (with & arrowHead .~ spike & arrowTail .~ quill) sPt nPt
          # centerXY
    sPt = p2 (0.50, 0.10)
    nPt = p2 (0.50, 0.70)

chessBoard' n
  = vcat . map hcat . map (map boardSq')
  . take n . map (take n) . tails
  $ cycle [saddlebrown, antiquewhite]

squareToPoint :: Square -> P2
squareToPoint (x,y) = (^&) (fromIntegral x) (negate (fromIntegral y))

example = chessBoard' 8

-- FIXME: The arguments were done for expediency

-- errChart :: Graphics.Rendering.Chart.Renderable ()
errChart xs mcMAvg trial trialInitState testData nitt = toRenderable layout
  where
    sinusoid1 = plot_lines_values .~ [[ (x, abs $ mcMAvg $
                                            trial trialInitState x (testData nitt))
                                      | x <- xs]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "error"
              $ def

    layout = layout_title .~ "Floating Point Error"
           $ layout_plots .~ [toPlot sinusoid1]
           $ layout_y_axis .~ errorAxis
           $ layout_x_axis .~ stepSizeAxis
           $ def

    errorAxis = laxis_title .~ "Minus log to base 2 of the error"
              $ def

    stepSizeAxis = laxis_title .~ "Minus log to base 2 of the step size"
                 $ def
