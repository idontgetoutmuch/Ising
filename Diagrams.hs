{-# LANGUAGE TypeFamilies                  #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams (
    example
  , example1
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
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Default.Class

import System.IO.Unsafe


type Square = (Int, Int)

board :: [Square]
board = [ (x,y) | x <- [0..7], y <- [0..7] ]

knightMoves :: Square -> [Square]
knightMoves (x,y) = filter (flip elem board) jumps
  where jumps = [ (x+i,y+j) | i <- jv, j <- jv, abs i /= abs j ]
        jv    = [1,-1,2,-2]

knightTour :: Square -> [Square]
knightTour sq = knightTour' [sq]
  where
    knightTour' moves@(lastMove:_)
        | null candMoves = reverse moves
        | otherwise = knightTour' $ newSquare : moves
      where newSquare   = minimumBy (comparing (length . findMoves)) candMoves
            candMoves   = findMoves lastMove
            findMoves s = knightMoves s \\ moves

boardSq' c = square 1 # lw 0 # fc c

chessBoard' n
  = vcat . map hcat . map (map boardSq')
  . take n . map (take n) . tails
  $ cycle [saddlebrown, antiquewhite]

squareToPoint :: Square -> P2
squareToPoint (x,y) = (^&) (fromIntegral x) (negate (fromIntegral y))

sq = square 2 # showOrigin # lc darkgray # lw 0.07
ds = (sq # named "left") ||| strutX 3 ||| (sq # named "right")

shaft  = cubicSpline False ( map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])

example1 = ds # connect' (with & arrowHead .~ dart & headSize .~ 0.6
                               & tailSize .~ 0.5 & arrowTail .~ quill
                               & shaftStyle %~ lw 0.02 & arrowShaft .~ shaft)
                               "left" "right" # pad 1.1

sPt = p2 (0.20, 0.20)
ePt = p2 (2.85, 0.85)

-- We use small blue and red circles to mark the start and end points.
dot  = circle 0.02 # lw 0
sDot = dot # fc blue # moveTo sPt
eDot = dot # fc red  # moveTo ePt

example2 = ( sDot <> eDot <> arrowBetween sPt ePt)
           # centerXY # pad 1.1

drawTour tour = tourPoints <> stroke tourPath
  where
    tourPath   = fromVertices . map squareToPoint $ tour
    tourPoints = decoratePath tourPath (repeat dot)
    dot = circle 0.05 # fc black

example =
  mconcat
  [ drawTour tour
  , chessBoard' 8
  ]
  where
    tourStart = (1,3)
    tour      = knightTour tourStart
    tourEnd   = last tour

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
