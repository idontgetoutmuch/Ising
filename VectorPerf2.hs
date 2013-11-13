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
import Diagrams.Prelude hiding ( sample )
import Diagrams.Coordinates ( (&) )
import Diagrams.Backend.Cairo.CmdLine

import Test.HUnit
import qualified Test.Tasty as T
import Test.Tasty.HUnit

import qualified Debug.Trace as D

data McState = McState { mcMagnetization :: !Double
                       , mcCount         :: !Int
                       , mcNumSamples    :: !Int
                       , mcGrid          :: !(V.Vector Int)
                       }
  deriving Show

gridSize :: Int
gridSize = 10

measure :: Int
measure = 1 -- 100

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
singleUpdate measure expDvT u (i, j, r) =
  McState { mcMagnetization =
               if (mcCount u) `mod` measure == 0
               then newMag
               else oldMag
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
  (pureMT 3)
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
                    , mcCount = 0
                    , mcNumSamples = 0
                    , mcGrid = initGrid'
                    }

trialInitState = McState { mcMagnetization = fromIntegral $
                                            magnetization trialGrid
                        , mcCount = 0
                        , mcNumSamples = 0
                        , mcGrid = trialGrid
                        }

trialInitState225 = McState { mcMagnetization = fromIntegral $
                                                magnetization trialGrid225
                            , mcCount = 0
                            , mcNumSamples = 0
                            , mcGrid = trialGrid225
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

trialGrid225 :: V.Vector Int
trialGrid225 = V.fromList $ concat $ initGridL
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
                , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]]


trialData225 :: V.Vector (Int, Int, Double)
trialData225 = V.fromList [ (9, 5, 0.346237909455)
                          , (6, 2, 0.913915477165)
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

{-
>>> [[-1 -1  1 -1 -1 -1 -1  1 -1 -1]
 [ 1  1 -1  1 -1  1  1 -1  1 -1]
 [ 1 -1 -1 -1 -1  1 -1 -1 -1 -1]
 [-1 -1  1 -1  1 -1  1  1 -1  1]
 [ 1  1  1 -1  1 -1 -1  1  1  1]
 [ 1 -1 -1  1 -1 -1 -1 -1  1  1]
 [ 1  1  1 -1 -1  1 -1 -1  1 -1]
 [ 1 -1 -1 -1  1  1 -1 -1  1 -1]
 [ 1 -1  1 -1 -1 -1 -1  1  1 -1]
 [-1 -1  1  1 -1 -1  1  1 -1  1]]
0
7 7
0.133954208172
1
6 8
0.748777878277
[[-1 -1  1 -1 -1 -1 -1  1 -1 -1]
 [ 1  1 -1  1 -1  1  1 -1  1 -1]
 [ 1 -1 -1 -1 -1  1 -1 -1 -1 -1]
 [-1 -1  1 -1  1 -1  1  1 -1  1]
 [ 1  1  1 -1  1 -1 -1  1  1  1]
 [ 1 -1 -1  1 -1 -1 -1 -1  1  1]
 [ 1  1  1 -1 -1  1 -1 -1 -1 -1]
 [ 1 -1 -1 -1  1  1 -1  1  1 -1]
 [ 1 -1  1 -1 -1 -1 -1  1  1 -1]
 [-1 -1  1  1 -1 -1  1  1 -1  1]]
4.0 -12.0 8.0
-}

{-
[[-1 -1  1 -1 -1 -1 -1  1 -1 -1]
 [ 1  1 -1  1 -1  1  1 -1  1 -1]
 [ 1 -1 -1 -1 -1  1 -1 -1 -1 -1]
 [-1 -1  1 -1  1 -1  1  1 -1  1]
 [ 1  1  1 -1  1 -1 -1  1  1  1]
 [ 1 -1 -1  1 -1 -1 -1 -1  1  1]
 [ 1  1  1 -1 -1  1 -1 -1 -1 -1]
 [ 1 -1 -1 -1  1  1 -1  1  1 -1]
 [ 1 -1  1 -1 -1 -1 -1  1  1 -1]
 [-1 -1  1  1 -1 -1  1  1 -1  1]]
0
9 5
0.346237909455
1
6 2
0.913915477165
[[-1 -1  1 -1 -1 -1 -1  1 -1 -1]
 [ 1  1 -1  1 -1  1  1 -1  1 -1]
 [ 1 -1 -1 -1 -1  1 -1 -1 -1 -1]
 [-1 -1  1 -1  1 -1  1  1 -1  1]
 [ 1  1  1 -1  1 -1 -1  1  1  1]
 [ 1 -1 -1  1 -1 -1 -1 -1  1  1]
 [ 1  1 -1 -1 -1  1 -1 -1 -1 -1]
 [ 1 -1 -1 -1  1  1 -1  1  1 -1]
 [ 1 -1  1 -1 -1 -1 -1  1  1 -1]
 [-1 -1  1  1 -1 -1  1  1 -1  1]]
2.25 -14.0 4.0
-}

main :: IO ()
main = do let newState = trial trialInitState 1.375 trialData
          let newState225 = trial trialInitState225 2.25 trialData225
          T.defaultMain $ T.testGroup "Two step updates"
            [ testCase "T = 1.375 Grid" $
              assertEqual "Grid" (mcGrid newState) expectedGrid
            , testCase "T = 1.375 Magnetization" $
              assertEqual "Magnetization" (mcMagnetization newState)
                                          expectedMagnetization
            , testCase "T = 2.25 Grid" $
              assertEqual "Grid" (mcGrid newState225) expectedGrid225
            , testCase "T = 2.25 Magnetization" $
              assertEqual "Magnetization" (mcMagnetization newState225)
                                          expectedMagnetization225
            ]
          
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
    
