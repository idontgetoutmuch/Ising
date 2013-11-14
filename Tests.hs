

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
