{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- import qualified System.Random.MWC as MWC-- ( create, uniformVector, asGenST )
import System.Random -- (mkStdGen )
import Data.Random
import Data.RVar
-- import System.Random.Mersenne.Pure64
import Data.Random.Source.PureMT
import Data.Random.Source.MWC

import Data.Int

import Data.Array.Repa hiding ( map )

import Control.Monad.State

import Data.List ( foldl' )

import Control.Monad.ST.Safe

import Control.Monad.Trans as MTL

gridSize :: Int
gridSize = 5

initGrid = fromListUnboxed (Z :. gridSize :. gridSize :: DIM2)
                           (take (gridSize * gridSize) $ cycle [1 :: Int, -1 ::Int])

t :: Double
t = 2.0 / log (1.0 + sqrt 2.0) - 0.1

rs = foldl' (+) 0 $
     map fromIntegral $
     fst $
     runState (replicateM (10^6) (sample (uniform (0 :: Int8) 1))) (pureMT 1)

rwalkState :: RVarT (State Double) Double
rwalkState = do
     prev <- MTL.lift get
     change  <- rvarT StdNormal
     
     let new = prev + change
     MTL.lift (put new)
     return new

rwalk :: Int -> Double -> StdGen -> ([Double], StdGen)
rwalk count start gen = 
  flip evalState start .
  flip runStateT gen .
  sampleRVarTWith MTL.lift $
  replicateM count rwalkState

foo = runState (replicateM 2 (sampleRVar (uniform 6 8))) (mkStdGen 1)
