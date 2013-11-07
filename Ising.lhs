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

Acknowledgements
----------------

-- James Cook's package and comments

Other
-----

The letter Z stands for the German word Zustandssumme, "sum over states"

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}

> import System.Random
> import Data.Random
> import Data.RVar
> import Data.Random.Source.PureMT
> import Data.Random.Source.MWC

> import Data.Int

> import Data.Array.Repa hiding ( map )
> import Data.Array.Repa.Eval
> import Data.Vector.Unboxed.Base

> import Control.Monad.State

> import Data.List ( foldl' )

> import Control.Monad.Trans as MTL


> import Control.Monad
> import Control.Monad.ST.Lazy
> import Data.List
> import Data.Int
> import Data.Random
> import qualified System.Random.MWC as MWC

> import Text.PrettyPrint
> import Text.PrettyPrint.HughesPJClass

> instance (Source t a, Pretty a) => Pretty (Array t DIM1 a) where
>   pPrint a = brackets $ hcat $ punctuate (comma <> space) elems
>     where
>       elems = [ pPrint (a!j) | i <- [0..n-1], let j = Z:. i ]
>       Z :. n = extent a

> instance (Source t a, Pretty a) => Pretty (Array t DIM2 a) where
>   pPrint a = vcat elems
>     where
>       elems = [ pPrint (slice a j) | i <- [0..n-1], let j = Any :. i :. All]
>       Z :. n :. _m = extent a

We go against tradition and make our grid asymmetric so that we can
test our solution against the analytic solution for the one
dimensional case.

> gridSizeR, gridSizeC :: Int
> gridSizeR = 7
> gridSizeC = 7

> initGrid = fromListUnboxed (Z :. gridSizeR :. gridSizeC :: DIM2) xs
>   where
>     xs = map fromIntegral $
>          evalState (replicateM (gridSizeR * gridSizeC) (sample (uniform (0 :: Int8) 1)))
>                    (pureMT 1)


> tCrit :: Double
> tCrit = 2.0 / log (1.0 + sqrt 2.0) - 0.1

> h :: Double
> h = 0.0

FIXME: Please don't use head!

> magnetization :: (Source r a, Elt a, Unbox a, Monad m, Num a) =>
>                  Array r DIM2 a -> m a
> magnetization a = do
>   sumP a >>= sumP >>= return . head . toList

-- Periodic boundary conditions

    #Calculate initial magnetization:
    M = spin_matrix.sum()


> rs = foldl' (+) 0 $
>      map fromIntegral $
>      fst $
>      runState (replicateM (10^6) (sample (uniform (0 :: Int8) 1))) (pureMT 1)

> rwalkState :: RVarT (State Double) Double
> rwalkState = do
>      prev <- MTL.lift get
>      change  <- rvarT StdNormal
>      
>      let new = prev + change
>      MTL.lift (put new)
>      return new

> rwalk :: Int -> Double -> StdGen -> ([Double], StdGen)
> rwalk count start gen = 
>   flip evalState start .
>   flip runStateT gen .
>   sampleRVarTWith MTL.lift $
>   replicateM count rwalkState

> foo = runState (replicateM 2 (sampleRVar (uniform 6 8))) (mkStdGen 1)

-- Don't do this. It is slow in and has a space leak in GHCi but not
-- with -O2

-- main = print $ sum $ map fromIntegral $ runST $ do
--     mwc <- strictToLazyST MWC.create
--     replicateM (10^8) (strictToLazyST (sampleFrom mwc (uniform (0 :: Int8) 1)))

-- ~/Dropbox/Private/Ising $ time ./Ising
-- 50000208

-- real	1m40.204s
-- user	1m39.398s
-- sys	0m0.802s

> main = print $
>        foldl' (+) 0 $
>        map fromIntegral $
>        fst $
>        runState (replicateM (10^8) (sample (uniform (0 :: Int8) 1))) (pureMT 1)

-- ~/Dropbox/Private/Ising $ time ./Ising
-- 50008163

-- real	0m18.462s
-- user	0m18.220s
-- sys	0m0.238s

