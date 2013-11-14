% Neural Networks and Automated Differentiation
% Dominic Steinitz
% 4th April 2013

---
bibliography: Ising.bib
---

Introduction
------------

About a year ago there was a reddit post on the [Ising Model in
Haskell](http://www.reddit.com/r/haskell/comments/16uc2x/ising_model_in_haskell). The
discussion seems to have fizzled out but Ising models look like a
perfect fit for Haskell using [repa](http://hackage.haskell.org/package/repa).

The [Ising model](http://en.wikipedia.org/wiki/Ising_model) was (by
[Stigler's
law](http://en.wikipedia.org/wiki/Stigler%27s_law_of_eponymy))
proposed by Lenz in 1920 as a model for ferromagnetism, that is, the
magnetism exhibited by bar magnets. The phenomenon ferromagnetism is
so named because it was first observed in iron (Latin ferrum and
chemical symbol Fe). It is also exhibited, for example, by rare earths
such as neodymium.

The Ising model can also be used to describe phase transitions in alloys.

Following Ziman [@Ziman:Principles], we assume that each atom in the
ferromagnetic material behaves like a small magnet.

This is good for ferromagnetism: http://en.wikipedia.org/wiki/Ferromagnetism and this contains Curie temperatures: http://hyperphysics.phy-astr.gsu.edu/hbase/solids/ferro.html

Quotes from http://arxiv.org/pdf/0905.1629.pdf:

the drosophila of statistical mechanics

embarrassingly parallel


Acknowledgements
----------------

-- James Cook's package and comments

Other
-----

An explanation of the Boltzmann distribution (which we need to replay):

http://www.math.fsu.edu/~quine/MB_11/10%20HP%20model%20and%20Boltzmann%20distribution.pdf

An explanation of the Ising model using the Boltzmann distribution: http://www.uio.no/studier/emner/matnat/fys/FYS3150/h07/undervisningsmateriale/Lecture%20Notes/lecture2007.pdf

The letter Z stands for the German word Zustandssumme, "sum over states"

Markov Chains
-------------

Let $S$ be a finite set. In the case of an Ising model with $N$ cells,
this set will contain $2^N$ elements. Let $P = \{ p_{ij} : i, j \in S
\}$.


Other Other
-----------

A warm start but we could try a cold start with all spins up.


Calculate magnetization:

Calculate energy:

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}
> 
> {-# LANGUAGE TypeFamilies                  #-}
> 
> module Ising (
>        McState (..)
>        , main -- FIXME: For now just to get rid of warnings
>        ) where
> 
> import qualified Data.Vector.Unboxed as V
> import qualified Data.Vector.Unboxed.Mutable as M
> import Data.Random.Source.PureMT
> import Data.Random
> import Control.Monad.State
> 
> import Data.List.Split ( chunksOf )
> import Diagrams.Prelude hiding ( sample, render )
> import qualified Diagrams.Prelude as D
> import Diagrams.Coordinates ( (&) )
> import Diagrams.Backend.Cairo.CmdLine
> 
> import Graphics.Rendering.Chart
> import Data.Default.Class
> import Graphics.Rendering.Chart.Backend.Cairo hiding (runBackend, defaultEnv)
> import Control.Lens hiding ( (#), (&) )
> data McState = McState { mcMagnetization :: !Double
>                        , mcMAvg          :: !Double
>                        , mcCount         :: !Int
>                        , mcNumSamples    :: !Int
>                        , mcGrid          :: !(V.Vector Int)
>                        }
>   deriving Show
> 
> gridSize :: Int
> gridSize = 10
> 
> measure :: Int
> measure = 100
> 
> nitt :: Int
> nitt = 1000000
> 
> tCrit :: Double
> tCrit = 2.0 / log (1.0 + sqrt 2.0) - 0.1
> 
> magnetization :: (V.Unbox a, Num a) => V.Vector a => a
> magnetization = V.sum
> 
> energy :: V.Vector Int => Double
> energy v = 0.5 * (fromIntegral $ V.sum energyAux)
>   where
> 
>     energyAux = V.generate l f
> 
>     l = V.length v
> 
>     f m = c * d
>       where
>         i = m `mod` gridSize
>         j = (m `mod` (gridSize * gridSize)) `div` gridSize
> 
>         c = v V.! jc
>         jc = gridSize * i + j
>         
>         d = n + e + s + w
> 
>         n = v V.! jn
>         e = v V.! je
>         s = v V.! js
>         w = v V.! jw
>     
>         jn = gridSize * ((i + 1) `mod` gridSize) + j
>         js = gridSize * ((i - 1) `mod` gridSize) + j
>         je = gridSize * i + ((j + 1) `mod` gridSize)
>         jw = gridSize * i + ((j - 1) `mod` gridSize)
>     
> expDv :: Double -> V.Vector Double
> expDv t = V.generate 9 f
>   where
>     f n | odd n = 0.0
>     f n         = exp (((fromIntegral (8 - n)) - 4.0) * 2.0 / t)
> 
> singleUpdate :: Int -> V.Vector Double -> McState -> (Int, Int, Double) -> McState
> singleUpdate measure expDvT u (i, j, r) = -- D.trace (show $ mcMAvg u) $ 
>   McState { mcMagnetization = newMag
>           , mcMAvg =
>             if (mcCount u) `mod` measure == 0
>             then mcMAvg u + newMag
>             else mcMAvg u
>           , mcCount = mcCount u + 1
>           , mcNumSamples =
>             if (mcCount u) `mod` measure == 0
>             then mcNumSamples u + 1
>             else mcNumSamples u
>           , mcGrid = newGrid
>           }
>   where
>     newGrid = if p > r
>               then V.modify (\v -> M.write v jc (-c)) v
>               else v
> 
>     oldMag = mcMagnetization u
>     
>     newMag = if p > r
>               then oldMag - 2 * (fromIntegral c)
>               else oldMag
>     
>     v = mcGrid u
>     
>     p = expDvT V.! (4 + c * d)
> 
>     c = v V.! jc
>     jc = gridSize * i + j
> 
>     d = n + e + s + w
> 
>     n = v V.! jn
>     e = v V.! je
>     s = v V.! js
>     w = v V.! jw
> 
>     jn = gridSize * ((i + 1) `mod` gridSize) + j
>     js = gridSize * ((i - 1) `mod` gridSize) + j
>     je = gridSize * i + ((j + 1) `mod` gridSize)
>     jw = gridSize * i + ((j - 1) `mod` gridSize)
> 
> testData :: Int -> V.Vector (Int, Int, Double)
> testData m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 2)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)            1.0)
>            return (r, c, v)
> 
> trial :: McState -> Double -> V.Vector (Int, Int, Double) -> McState
> trial s t = V.foldl (singleUpdate 1 (expDv t)) s
> 
> trialInitState :: McState
> trialInitState = McState { mcMagnetization = fromIntegral $
>                                              magnetization trialGrid
>                          , mcMAvg = 0.0
>                          , mcCount = 0
>                          , mcNumSamples = 0
>                          , mcGrid = trialGrid
>                         }
> 
> trialGrid :: V.Vector Int
> trialGrid = V.fromList $ concat $ initGridL
>   where
>     initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
>                 , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
>                 , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
>                 , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
>                 , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
>                 , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
>                 , [ 1,  1,  1, -1, -1,  1, -1, -1,  1, -1]
>                 , [ 1, -1, -1, -1,  1,  1, -1, -1,  1, -1]
>                 , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
>                 , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
>                 ]
> 
> xs :: [Double]
> xs = getTemps 4.0 0.5 100
> 
> getTemps :: Double -> Double -> Int -> [Double]
> getTemps h l n = [ m * x + c |
>                    w <- [1..n],
>                    let x = fromIntegral w ]
>   where
>     m = (h - l) / (fromIntegral n - 1)
>     c = l - m
> 
> newGrids :: [McState]
> newGrids = map (\t -> trial trialInitState t (testData nitt)) xs
> 
> main :: IO ()
> main = do print "Magnetization"
>           mapM_ putStrLn $
>             zipWith (\t x -> show t ++ " " ++
>                              show (mcMAvg x / fromIntegral nitt)) xs newGrids
> 
>           renderableToPNGFile errChart 500 500 "Magnetism.png"
>           defaultMain $
>             (chessBoard (mcGrid $ newGrids!!0) # D.translate (0&0)) <>
>             (chessBoard (mcGrid $ newGrids!!1) # D.translate (12&0))
>           
> boardSq :: (Transformable b, HasStyle b, TrailLike b, V b ~ R2) =>
>            Colour Double -> b
> boardSq c = square 1 # lw 0 # fc c
> 
> chessBoard :: (Monoid c, Semigroup c, Transformable c, HasStyle c,
>                Juxtaposable c, HasOrigin c, TrailLike c, V c ~ R2) =>
>               V.Vector Int -> c
> chessBoard v
>   = vcat $ map hcat $ map (map boardSq)
>   $ chunksOf gridSize $ map f $ V.toList v
>   where
>     f (-1) = red
>     f   1  = blue
>     f _    = error "Unexpected spin"
> 
> errChart :: Graphics.Rendering.Chart.Renderable ()
> errChart = toRenderable layout
>   where
>     sinusoid1 = plot_lines_values .~ [[ (x, abs $ mcMAvg $
>                                             trial trialInitState x (testData nitt))
>                                       | x <- xs]]
>               $ plot_lines_style  . line_color .~ opaque blue
>               $ plot_lines_title .~ "error"
>               $ def
> 
>     layout = layout1_title .~ "Floating Point Error"
>            $ layout1_plots .~ [Left (toPlot sinusoid1)]
>            $ layout1_left_axis .~ errorAxis
>            $ layout1_bottom_axis .~ stepSizeAxis
>            $ def
> 
>     errorAxis = laxis_title .~ "Minus log to base 2 of the error"
>               $ def
> 
>     stepSizeAxis = laxis_title .~ "Minus log to base 2 of the step size"
>                  $ def
> 
> 
> testData' :: Int -> V.Vector (Int, Int, Double)
> testData' m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 1)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)           1.0)
>            return (r, c, v)
> 

