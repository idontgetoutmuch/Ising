{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

{-# LANGUAGE TypeFamilies #-}

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Random.Source.PureMT
import Data.Random
import Control.Monad.State

import Data.List.Split ( chunksOf )
import Diagrams.Prelude hiding ( sample, render )
import Diagrams.Coordinates ( (&) )
import Diagrams.Backend.Cairo.CmdLine

import Test.HUnit
import qualified Test.Tasty as T
import Test.Tasty.HUnit

import qualified Debug.Trace as D

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.Monoid
import Graphics.Rendering.Chart.Backend.Cairo hiding (runBackend, defaultEnv)
import Graphics.Rendering.Chart.Backend.Diagrams
import Diagrams.Core.Types hiding ( render, sample )
import Diagrams.TwoD.Types
import Diagrams.Backend.Cairo
import Control.Lens hiding ( (#) )

import System.IO.Unsafe


data McState = McState { mcMagnetization :: !Double
                       , mcMAvg          :: !Double
                       , mcCount         :: !Int
                       , mcNumSamples    :: !Int
                       , mcGrid          :: !(V.Vector Int)
                       }
  deriving Show

gridSize :: Int
gridSize = 10

measure :: Int
measure = 100

nitt :: Int
nitt = 1000000

tCrit :: Double
tCrit = 2.0 / log (1.0 + sqrt 2.0) - 0.1

magnetization :: (V.Unbox a, Num a) => V.Vector a => a
magnetization = V.sum

energy :: V.Vector Int => Double
energy v = 0.5 * (fromIntegral $ V.sum energyAux)
  where

    energyAux = V.generate l f

    l = V.length v

    f m = c * d
      where
        i = m `mod` gridSize
        j = (m `mod` (gridSize * gridSize)) `div` gridSize

        c = v V.! jc
        jc = gridSize * i + j
        
        d = n + e + s + w

        n = v V.! jn
        e = v V.! je
        s = v V.! js
        w = v V.! jw
    
        jn = gridSize * ((i + 1) `mod` gridSize) + j
        js = gridSize * ((i - 1) `mod` gridSize) + j
        je = gridSize * i + ((j + 1) `mod` gridSize)
        jw = gridSize * i + ((j - 1) `mod` gridSize)
    
expDv :: Double -> V.Vector Double
expDv t = V.generate 9 f
  where
    f n | odd n = 0.0
    f n         = exp (((fromIntegral (8 - n)) - 4.0) * 2.0 / t)

singleUpdate :: Int -> V.Vector Double -> McState -> (Int, Int, Double) -> McState
singleUpdate measure expDvT u (i, j, r) = -- D.trace (show $ mcMAvg u) $ 
  McState { mcMagnetization = newMag
          , mcMAvg =
            if (mcCount u) `mod` measure == 0
            then mcMAvg u + newMag
            else mcMAvg u
          , mcCount = mcCount u + 1
          , mcNumSamples =
            if (mcCount u) `mod` measure == 0
            then mcNumSamples u + 1
            else mcNumSamples u
          , mcGrid = newGrid
          }
  where
    newGrid = if p > r
              then V.modify (\v -> M.write v jc (-c)) v
              else v

    oldMag = mcMagnetization u
    
    newMag = if p > r
              then oldMag - 2 * (fromIntegral c)
              else oldMag
    
    v = mcGrid u
    
    p = expDvT V.! (4 + c * d)

    c = v V.! jc
    jc = gridSize * i + j

    d = n + e + s + w

    n = v V.! jn
    e = v V.! je
    s = v V.! js
    w = v V.! jw

    jn = gridSize * ((i + 1) `mod` gridSize) + j
    js = gridSize * ((i - 1) `mod` gridSize) + j
    je = gridSize * i + ((j + 1) `mod` gridSize)
    jw = gridSize * i + ((j - 1) `mod` gridSize)

testData :: Int -> V.Vector (Int, Int, Double)
testData m =
  V.fromList $
  evalState (replicateM m x)
  (pureMT 2)
  where
    x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
           c <- sample (uniform (0 :: Int)    (gridSize - 1))
           v <- sample (uniform (0 :: Double)            1.0)
           return (r, c, v)

initGrid :: V.Vector Int
initGrid = V.replicate (gridSize * gridSize) (1)

initGrid' :: V.Vector Int
initGrid' = V.fromList xs
  where
    xs = map (\x -> 2 * x - 1) $
         evalState (replicateM (gridSize * gridSize) (sample (uniform (0 :: Int) 1)))
                   (pureMT 1)


trial :: McState -> Double -> V.Vector (Int, Int, Double) -> McState
trial s t = V.foldl (singleUpdate 1 (expDv t)) s

trial' :: Double -> V.Vector (Int, Int, Double) -> McState
trial' t = V.foldl (singleUpdate measure (expDv t)) trialInitState -- initState

initState = McState { mcMagnetization = fromIntegral $
                                        magnetization initGrid'
                    , mcMAvg = 0.0
                    , mcCount = 0
                    , mcNumSamples = 0
                    , mcGrid = initGrid'
                    }

trialInitState = McState { mcMagnetization = fromIntegral $
                                             magnetization trialGrid
                         , mcMAvg = 0.0
                         , mcCount = 0
                         , mcNumSamples = 0
                         , mcGrid = trialGrid
                        }

trialGrid :: V.Vector Int
trialGrid = V.fromList $ concat $ initGridL
  where
    initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
                , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
                , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
                , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
                , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
                , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
                , [ 1,  1,  1, -1, -1,  1, -1, -1,  1, -1]
                , [ 1, -1, -1, -1,  1,  1, -1, -1,  1, -1]
                , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
                , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
                ]

trialData :: V.Vector (Int, Int, Double)
trialData = V.fromList [ (7, 7, 0.133954208172)
                       , (6, 8, 0.748777878277)
                       ]


-- (6, 8) and (7, 7) are flipped so the magnetization remains constant.

expectedGrid :: V.Vector Int
expectedGrid = V.fromList $ concat $ initGridL
  where
    initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
                , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
                , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
                , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
                , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
                , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
                , [ 1,  1,  1, -1, -1,  1, -1, -1, -1, -1]
                , [ 1, -1, -1, -1,  1,  1, -1,  1,  1, -1]
                , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
                , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
                ]

expectedMagnetization :: Double
expectedMagnetization = -12.0

expectedGrid225 :: V.Vector Int
expectedGrid225 = V.fromList $ concat $ initGridL
  where
    initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
                , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
                , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
                , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
                , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
                , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
                , [ 1,  1, -1, -1, -1,  1, -1, -1, -1, -1]
                , [ 1, -1, -1, -1,  1,  1, -1,  1,  1, -1]
                , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
                , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
                ]

expectedMagnetization225 :: Double
expectedMagnetization225 = -14.0

trialDataLong400 :: V.Vector (Int, Int, Double)
trialDataLong400 = V.fromList
  [ (7, 7, 0.133954208172)
  , (6, 8, 0.748777878277)
  , (9, 7, 0.543345230584)
  , (2, 2, 0.91845864659)
  , (9, 5, 0.346237909455)
  , (6, 2, 0.913915477165)
  , (1, 4, 0.540191515667)
  , (0, 6, 0.826249828434)
  , (2, 6, 0.176712161584)
  , (9, 5, 0.489266166995)
  ]

initGridLong400 :: V.Vector Int
initGridLong400 = V.fromList $ concat $ initGridL
  where
    initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
                , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
                , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
                , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
                , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
                , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
                , [ 1,  1,  1, -1, -1,  1, -1, -1,  1, -1]
                , [ 1, -1, -1, -1,  1,  1, -1, -1,  1, -1]
                , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
                , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
                ]

finalGridLong400 :: V.Vector Int
finalGridLong400 = V.fromList $ concat $ initGridL
  where
    initGridL = [  [-1, -1,  1, -1, -1, -1,  1,  1, -1, -1]
                 , [ 1,  1, -1,  1,  1,  1,  1, -1,  1, -1]
                 , [ 1, -1, -1, -1, -1,  1,  1, -1, -1, -1]
                 , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
                 , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
                 , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
                 , [ 1,  1, -1, -1, -1,  1, -1, -1, -1, -1]
                 , [ 1, -1, -1, -1,  1,  1, -1,  1,  1, -1]
                 , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
                 , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
                ]

initStateLong400 = McState { mcMagnetization = fromIntegral $
                                               magnetization initGridLong400
                           , mcMAvg = 0.0
                           , mcCount = 0
                           , mcNumSamples = 0
                           , mcGrid = initGridLong400
                           }

expectedMagnetizationLong400 :: Double
expectedMagnetizationLong400 = -10.0

m = (4.0 - 0.5) / (100 - 1)
c = 0.5 - m
xs = map (\x -> m * x + c) [1..100]

newGrids = map (\t -> trial trialInitState t (testData nitt)) xs

main :: IO ()
main = do let newState = trial trialInitState 1.375 trialData
              newStateLong400 = trial initStateLong400 4.00 trialDataLong400
          print $ mcMagnetization newStateLong400
          print $ magnetization finalGridLong400
          print $ mcMAvg newStateLong400 / (fromIntegral $ V.length trialDataLong400)
          -- let newGrid0 = trial trialInitState 0.50  (testData nitt)
              -- newGrid1 = trial trialInitState 1.375 (testData nitt)
              -- newGrid2 = trial trialInitState 2.25  (testData nitt)
              -- newGrid3 = trial trialInitState 3.125 (testData nitt)
              -- newGrid4 = trial trialInitState 4.00  (testData nitt)
          print "Magnetization"
          mapM_ putStrLn $
            zipWith (\t x -> show t ++ " " ++
                             show (mcMAvg x / fromIntegral nitt)) xs newGrids

          
          -- env <- defaultEnv vectorAlignmentFns 500 500
          -- let d :: QDiagram Cairo R2 Any
          --     d = fst $ runBackend env (render errChart (500, 500))
          renderableToPNGFile errChart 500 500 "Magnetism.png"
          putStrLn "Hello"
          
          -- print $ mcMAvg newGrid0 / fromIntegral nitt
          -- print $ mcMAvg newGrid1 / fromIntegral nitt
          -- print $ mcMAvg newGrid2 / fromIntegral nitt
          -- print $ mcMAvg newGrid3 / fromIntegral nitt
          -- print $ mcMAvg newGrid4 / fromIntegral nitt
          
          -- defaultMain $ (chessBoard initGrid' # translate (0&0)) <>
          --               (chessBoard (mcGrid newGrid1) # translate (12&0)) <>
          --               (chessBoard (mcGrid newGrid2) # translate (24&0)) <>
          --               (chessBoard (mcGrid newGrid3) # translate (36&0)) <>
          --               (chessBoard (mcGrid newGrid4) # translate (48&0))
                        
          -- T.defaultMain $ T.testGroup "Two step updates"
          --   [ testCase "T = 1.375 Grid" $
          --     assertEqual "Grid" (mcGrid newState) expectedGrid
          --   , testCase "T = 1.375 Magnetization" $
          --     assertEqual "Magnetization" (mcMagnetization newState)
          --                                 expectedMagnetization
          --   , testCase "T = 2.25 Grid" $
          --     assertEqual "Grid" (mcGrid newState225) expectedGrid225
          --   , testCase "T = 2.25 Magnetization" $
          --     assertEqual "Magnetization" (mcMagnetization newState225)
          --                                 expectedMagnetization225
          --   , testCase "T = 4.00 Grid" $
          --     assertEqual "Grid" (mcGrid newStateLong400) finalGridLong400
              
          --   , testCase "T = 4.00 Magnetization" $
          --     assertEqual "Magnetization"
          --     expectedMagnetizationLong400
          --     (mcMAvg newStateLong400 / (fromIntegral $ V.length trialDataLong400))
          --   ]
          
-- main = do let newGrid1 = trial 1.0 (testData 1000000)
--               newGrid2 = trial 2.0 (testData 1000000)
--               newGrid3 = trial 3.0 (testData 1000000)
--               newGrid4 = trial 4.0 (testData 1000000)
--           defaultMain $ (chessBoard initGrid' # translate (0&0)) <>
--                         (chessBoard (mcGrid newGrid1) # translate (12&0)) <>
--                         (chessBoard (mcGrid newGrid2) # translate (24&0)) <>
--                         (chessBoard (mcGrid newGrid3) # translate (36&0)) <>
--                         (chessBoard (mcGrid newGrid4) # translate (48&0))
                        
boardSq :: (Transformable b, HasStyle b, TrailLike b, V b ~ R2) =>
           Colour Double -> b
boardSq c = square 1 # lw 0 # fc c

chessBoard :: (Monoid c, Semigroup c, Transformable c, HasStyle c,
               Juxtaposable c, HasOrigin c, TrailLike c, V c ~ R2) =>
              V.Vector Int -> c
chessBoard v
  = vcat $ map hcat $ map (map boardSq)
  $ chunksOf gridSize $ map f $ V.toList v
  where
    f (-1) = red
    f   1  = blue
    f _    = error "Unexpected spin"

bar :: DEnv
bar = unsafePerformIO $ defaultEnv vectorAlignmentFns 500 500

errDiag :: QDiagram Cairo R2 Any
errDiag = fst $ runBackend bar (render errChart (500, 500))
    
errChart = toRenderable layout
  where
    sinusoid1 = plot_lines_values .~ [[ (x, abs $ mcMAvg $
                                            trial trialInitState x (testData nitt))
                                      | x <- xs]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "error"
              $ def

    f  = negate . logBase 2
    g  = f . numericalErr 1.0

    layout = layout1_title .~ "Floating Point Error"
           $ layout1_plots .~ [Left (toPlot sinusoid1)]
           $ layout1_left_axis .~ errorAxis
           $ layout1_bottom_axis .~ stepSizeAxis
           $ def

    errorAxis = laxis_title .~ "Minus log to base 2 of the error"
              $ def

    stepSizeAxis = laxis_title .~ "Minus log to base 2 of the step size"
                 $ def


f :: Double -> Double
f x = exp x

numericalF' :: Double -> Double -> Double
numericalF' x h = (f (x + h) - f x) / h

numericalErr :: Double -> Double -> Double
numericalErr x h = abs ((exp 1.0 - numericalF' x h))

powersOf2 :: Fractional a => [a]
powersOf2 = 1 : map (/2) powersOf2

errs = map (logBase 2 . numericalErr 1.0) powersOf2
