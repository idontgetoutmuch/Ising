{-# LANGUAGE TypeFamilies                  #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams (
  example
  ) where

import Diagrams.Prelude
import Diagrams.Coordinates ( (&) )

import Data.List (minimumBy, tails, (\\))
import Data.Ord (comparing)

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
