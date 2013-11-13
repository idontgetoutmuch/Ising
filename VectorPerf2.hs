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

singleUpdate :: V.Vector Double -> McState -> (Int, Int, Double) -> McState
singleUpdate expDvT u (i, j, r) = McState { mcMagnetization =
                                               if (mcCount u) `mod` measure == 0
                                               then mcMagnetization u -
                                                    2 * (fromIntegral c)
                                               else mcMagnetization u
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


trial :: Double -> V.Vector (Int, Int, Double) -> McState
trial t = V.foldl (singleUpdate (expDv t)) testInitState -- initState

initState = McState { mcMagnetization = fromIntegral $
                                        magnetization initGrid'
                    , mcCount = 0
                    , mcNumSamples = 0
                    , mcGrid = initGrid'
                    }

testInitState = McState { mcMagnetization = fromIntegral $
                                            magnetization testGrid
                        , mcCount = 0
                        , mcNumSamples = 0
                        , mcGrid = testGrid
                        }

testGrid :: V.Vector Int
testGrid = V.fromList $ concat $ initGridL
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

testData2 :: V.Vector (Int, Int, Double)
testData2 = V.fromList [ (7, 7, 0.133954208172)
                       , (6, 8, 0.748777878277)
                       ]

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

foo :: Assertion
foo = assertEqual "Test" "hello" "world"

main :: IO ()
main = do print testInitState
          let newState = trial 1.375 testData2 -- (testData nitt)
          print newState
          print (mcMagnetization newState / fromIntegral (mcNumSamples newState))
          T.defaultMain $
            testCase "Two steps" $
            assertEqual "Test" (mcGrid newState) expectedGrid
          
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
    
