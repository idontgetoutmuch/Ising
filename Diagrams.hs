{-# LANGUAGE TypeFamilies                  #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams (
    example
  , errChart
  ) where

import Diagrams.Prelude
import Diagrams.Coordinates ( (&) )

import Data.List (minimumBy, tails, (\\))
import Data.Ord (comparing)

import Control.Lens hiding ( (#), (&), moveTo )
import Graphics.Rendering.Chart hiding ( moveTo )
import Graphics.Rendering.Chart.Backend.Cairo hiding (runBackend, defaultEnv)
import Data.Default.Class


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
squareToPoint (x,y) = (&) (fromIntegral x) (negate (fromIntegral y))

knight sq
  = circle 1.0
  # moveTo (squareToPoint sq)

drawTour tour = tourPoints <> stroke tourPath
  where
    tourPath   = fromVertices . map squareToPoint $ tour
    tourPoints = decoratePath tourPath (repeat dot)
    dot = circle 0.05 # fc black

example =
  mconcat
  [ knight tourStart
  , knight tourEnd
  , drawTour tour
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

    layout = layout1_title .~ "Floating Point Error"
           $ layout1_plots .~ [Left (toPlot sinusoid1)]
           $ layout1_left_axis .~ errorAxis
           $ layout1_bottom_axis .~ stepSizeAxis
           $ def

    errorAxis = laxis_title .~ "Minus log to base 2 of the error"
              $ def

    stepSizeAxis = laxis_title .~ "Minus log to base 2 of the step size"
                 $ def
