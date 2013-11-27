import qualified Data.Vector as V
 
test :: V.Vector Int -> Double
test = V.foldl (\ a b -> a * sqrt (fromIntegral b)) 0
 
create :: Int -> V.Vector Int
create n = (V.enumFromTo 1 n)
 
main = print (test (create 1000000))

{-
And after optimization (revealed with the ghc-core tool), we have only one loop:

main_$s$wfoldlM_loop :: Int# -> Double# -> Double#
 
main_$s$wfoldlM_loop =
  \ (sc_sWA :: Int#) (sc1_sWB :: Double#) ->
    case <=# sc_sWA 1000000 of _ {
      False -> sc1_sWB;
      True ->
        main_$s$wfoldlM_loop
          (+# sc_sWA 1)
          (*##
             sc1_sWB (sqrtDouble# (int2Double# sc_sWA)))
    }
-}