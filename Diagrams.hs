{-# LANGUAGE TypeFamilies                  #-}

{-# LANGUAGE NoMonomorphismRestriction     #-}
{-# LANGUAGE FlexibleContexts              #-}

module Diagrams (
    errChart
  , isingGrid
  , eFlipD
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
import Data.Default.Class
import qualified Data.Vector.Unboxed as V
import Data.List.Split ( chunksOf )

eFlipD before after = ds # connectPerim' arrowStyle1
                                         "2" "1" (5/12 :: Turn) (0 :: Turn)
   where

     ds = (isingEnergyFlip before # named "1") |||
          strutX 3 |||
          (isingEnergyFlip after # named "2")

     arrowStyle1 = (with  & arrowHead  .~ noHead & tailSize .~ 0.5
                          & arrowShaft .~ shaft  & arrowTail .~ spike'
                          & tailColor  .~ blue)

     shaft = P.arc 0 (1/6 :: Turn)

gridSq ec = case ec of
  Left  c ->            square 1 # lw 0 # fc (getColour c)
  Right c -> (arr c) <> square 1 # lw 0 # fc (getColour c)
  where
    arr c = arrowBetween' (with & arrowHead .~ spike & arrowTail .~ quill) (sPt c) (nPt c)
            # centerXY
    sPt x | x == -1 = p2 (0.50, 0.70)
    sPt x | x ==  1 = p2 (0.50, 0.10)
    sPt x           = error $ "Spins can be up or down: " ++ show x

    nPt x | x == -1 = p2 (0.50, 0.10)
    nPt x | x ==  1 = p2 (0.50, 0.70)
    nPt x           =  error $ "Spins can be up or down: " ++ show x

    getColour x | x == -1 = saddlebrown
    getColour x | x ==  1 = antiquewhite
    getColour x           = white

isingGrid :: P.Renderable (P.Path R2) b => Int -> V.Vector Int -> Diagram b R2
isingGrid n vs = if aLen == sLen
                 then result
                 else error $ "Specified grid size " ++ show sLen ++
                              " Actual grid size "   ++ show aLen
  where
    aLen = V.length vs
    sLen = n * n
    result = vcat $
             map hcat $
             map (map gridSq) $
             map (map Right) $
             chunksOf n $
             V.toList vs

isingEnergyFlip :: P.Renderable (P.Path R2) b => [Int] -> Diagram b R2
isingEnergyFlip vs = if aLen == 9
                     then result
                     else error $ " Actual grid size "   ++ show aLen ++
                                  " Should be 9"
  where
    aLen = length vs
    result = vcat $
             map hcat $
             map (map gridSq) $
             chunksOf 3 $
             zipWith f vs [0..]
    f x n | n `elem` [0, 2, 6, 8] = Left  x
    f x n                         = Right x

errChart xs rs f g h = toRenderable layout
  where
    p1 = plot_lines_values .~ [zipWith (\x y -> (x, f y)) xs rs]
         $ plot_lines_style  . line_color .~ opaque blue
         $ plot_lines_title .~ "Magnetization"
         $ def

    p2 = plot_lines_values .~ [zipWith (\x y -> (x, g y)) xs rs]
         $ plot_lines_style  . line_color .~ opaque red
         $ plot_lines_title .~ "Energy"
         $ def

    p3 = plot_lines_values .~ [zipWith (\x y -> (x, h y / x^2)) xs rs]
         $ plot_lines_style  . line_color .~ opaque green
         $ plot_lines_title .~ "Specific Heat"
         $ def

    layout = layout_title .~ "Ferromagnetism (Ising Model)"
           $ layout_plots .~ [toPlot p1, toPlot p2, toPlot p3]
           $ layout_y_axis .~ errorAxis
           $ layout_x_axis .~ stepSizeAxis
           $ def

    errorAxis = laxis_title .~ "Magnetization / (mu N^2), Energy / (J N^2), Specific Heat / (k_B N^2)"
              $ def

    stepSizeAxis = laxis_title .~ "Temperature * k_B / J"
                 $ def
