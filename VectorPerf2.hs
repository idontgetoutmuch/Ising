import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.Int
import Data.Random.Source.PureMT
import Data.Random
import Control.Monad.State

gridSize = 100
gridSize2 = gridSize * gridSize

singleUpdate :: V.Vector Int -> (Int, Int, Double) -> V.Vector Int
singleUpdate v (i, j, r) = if (fromIntegral (c * d)) > r
                           then V.modify (\v -> M.write v jc (-c)) v
                           else v
  where
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

testData'' :: Int -> V.Vector (Int, Int, Double)
testData'' m =
  V.fromList $
  evalState (replicateM m x)
  (pureMT 1)
  where
    x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
           c <- sample (uniform (0 :: Int)    (gridSize - 1))
           v <- sample (uniform (0 :: Double)            1.0)
           return (r, c, v)

testData :: Int -> V.Vector (Int, Int, Double)
testData m = V.generate m f
  where
    f n = (n `mod` gridSize,
           (n `mod` gridSize2) `div` gridSize,
           fromIntegral (n `mod` gridSize2) / (fromIntegral gridSize2)
          )

initGrid = V.replicate (gridSize * gridSize) (-1)

test = V.foldl singleUpdate initGrid

main = print (test (testData 100000000))
