Module Main (
       main
       ) where

import Ising
import Test.HUnit
import qualified Test.Tasty as T
import Test.Tasty.HUnit

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

trialInitState225 = McState { mcMagnetization = fromIntegral $
                                                magnetization trialGrid225
                            , mcMAvg = 0.0
                            , mcCount = 0
                            , mcNumSamples = 0
                            , mcGrid = trialGrid225
                        }

              newState225 = trial trialInitState225 2.25 trialData225
              newState225 = trial trialInitState225 2.25 trialData225

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

              newStateLong400 = trial initStateLong400 4.00 trialDataLong400
          print $ mcMagnetization newStateLong400
          print $ magnetization finalGridLong400
          print $ mcMAvg newStateLong400 / (fromIntegral $ V.length trialDataLong400)

trialData :: V.Vector (Int, Int, Double)
trialData = V.fromList [ (7, 7, 0.133954208172)
                       , (6, 8, 0.748777878277)
                       ]

initState = McState { mcMagnetization = fromIntegral $
                                        magnetization initGrid'
                    , mcMAvg = 0.0
                    , mcCount = 0
                    , mcNumSamples = 0
                    , mcGrid = initGrid'
                    }
initGrid' :: V.Vector Int
initGrid' = V.fromList xs
  where
    xs = map (\x -> 2 * x - 1) $
         evalState (replicateM (gridSize * gridSize) (sample (uniform (0 :: Int) 1)))
                   (pureMT 1)
trial' :: Double -> V.Vector (Int, Int, Double) -> McState
trial' t = V.foldl (singleUpdate measure (expDv t)) trialInitState -- initState


main = do
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
      
    , testCase "T = 4.00 Grid" $
      assertEqual "Grid" (mcGrid newStateLong400) finalGridLong400
      
    , testCase "T = 4.00 Magnetization" $
      assertEqual "Magnetization"
      expectedMagnetizationLong400
      (mcMAvg newStateLong400 / (fromIntegral $ V.length trialDataLong400))
    ]
