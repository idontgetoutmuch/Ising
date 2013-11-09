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
> {-# LANGUAGE FlexibleContexts #-}

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}

FIXME: Do we really need *all* these exports?

> module Ising (
>     updateOnce
>   , expDv
>   , energy
>   , magnetization
>   , h
>   , tCrit
>   , initGrid
>   , initGrid'
>   ) where

> -- import System.Random
> import Data.Random
> -- import Data.RVar
> import Data.Random.Source.PureMT
> -- import Data.Random.Source.MWC
> 
> import Data.Int

> import Data.Array.Repa hiding ( map, zipWith )
> import Data.Array.Repa.Eval
> import Data.Vector.Unboxed.Base

> import qualified Data.Vector as V

> import Control.Monad.State

> -- import Data.List ( foldl' )

> -- import Control.Monad
> -- import Control.Monad.ST.Lazy
> 
> -- import Data.List
> -- import Data.Random
> -- import qualified System.Random.MWC as MWC

> import Text.PrettyPrint
> import Text.PrettyPrint.HughesPJClass

> instance (Source t a, Pretty a) => Pretty (Array t DIM0 a) where
>   pPrint a = pPrint (a!Z)
>   
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
> gridSizeR = 10 -- 7
> gridSizeC = 10 -- 7


A warm start but we could try a cold start with all spins up.

> initGrid :: Array U DIM2 Double
> initGrid = fromListUnboxed (Z :. gridSizeR :. gridSizeC :: DIM2) xs
>   where
>     xs = map fromIntegral $
>          evalState (replicateM (gridSizeR * gridSizeC) (sample (uniform (0 :: Int8) 1)))
>                    (pureMT 1)

Start energy =  [[ 1 -1 -1 -1  1 -1  1  1  1  1]
 [-1 -1  1  1  1 -1  1  1  1  1]
 [ 1  1  1 -1 -1 -1 -1 -1 -1 -1]
 [-1  1  1 -1 -1 -1 -1  1 -1  1]
 [-1 -1  1 -1 -1 -1 -1 -1 -1  1]
 [-1 -1  1  1  1 -1  1 -1 -1  1]
 [-1 -1  1 -1 -1 -1  1  1 -1  1]
 [ 1  1 -1 -1 -1  1 -1 -1  1  1]
 [ 1 -1 -1 -1 -1  1  1 -1  1  1]
 [ 1 -1 -1 -1  1  1  1  1 -1 -1]] -36.0

> initGrid' :: Array U DIM2 Double
> initGrid' = fromListUnboxed (Z :. gridSizeR :. gridSizeC :: DIM2) $
>             concat
>             [ [ 1, -1, -1, -1,  1, -1,  1,  1,  1,  1]
>             , [-1, -1,  1,  1,  1, -1,  1,  1,  1,  1]
>             , [ 1,  1,  1, -1, -1, -1, -1, -1, -1, -1]
>             , [-1,  1,  1, -1, -1, -1, -1,  1, -1,  1]
>             , [-1, -1,  1, -1, -1, -1, -1, -1, -1,  1]
>             , [-1, -1,  1,  1,  1, -1,  1, -1, -1,  1]
>             , [-1, -1,  1, -1, -1, -1,  1,  1, -1,  1]
>             , [ 1,  1, -1, -1, -1,  1, -1, -1,  1,  1]
>             , [ 1, -1, -1, -1, -1,  1,  1, -1,  1,  1]
>             , [ 1, -1, -1, -1,  1,  1,  1,  1, -1, -1]]

> tCrit :: Double
> tCrit = 2.0 / log (1.0 + sqrt 2.0) - 0.1

> h :: Double
> h = 0.0

Calculate magnetization:

> magnetization :: (Source r a, Elt a, Unbox a, Monad m, Num a) =>
>                  Array r DIM2 a -> m a
> magnetization a =
>   sumP a >>= sumP >>= return . (!Z)

FIXME: Check that we are really summing the columns first.

Calculate energy:

> energy :: (Source r a, Elt a, Unbox a, Monad m, Num a, Fractional a) =>
>           Array r DIM2 a -> m a
> energy a = do
>   rSum <- sumP $ energyAux a
>   cSum <- sumP rSum
>   return (cSum!Z / 2)
>     where
>       energyAux a = traverse a id f
>         where
>           (Z :. nRows :. nCols) = extent a
>           f get (Z :. ir :. ic) = current * neighbours
>             where
>               current = get (Z :. ir :. ic)
>               neighbours = west + east + south + north
>               west       = get (Z :. ir                   :. (ic - 1) `mod` nCols)
>               east       = get (Z :. ir                   :. (ic + 1) `mod` nCols)
>               south      = get (Z :. (ir - 1) `mod` nRows :. ic)
>               north      = get (Z :. (ir + 1) `mod` nRows :. ic)

> expDv :: Double -> V.Vector Double
> expDv t = V.generate 9 f
>   where
>     f n | odd n = 0.0
>     f n         = exp (((fromIntegral (8 - n)) - 4.0) * 2.0 / t)

> updateOnce :: Source r a =>
>               [Double] -> [(Int, Int)] -> Array r DIM2 a -> Array D DIM2 Double
> updateOnce _rs _ijs a = traverse a id rutgers
>   where
>     (Z :. nRows :. nCols) = extent a
>     rutgers get (Z :. ir :. ic) = undefined
>       where
>         _oc = get (Z :. ir                   :. ic)
>         _w = get (Z :. ir                   :. (ic - 1) `mod` nCols)
>         _e = get (Z :. ir                   :. (ic + 1) `mod` nCols)
>         _s = get (Z :. (ir - 1) `mod` nRows :. ic)
>         _n = get (Z :. (ir + 1) `mod` nRows :. ic)
>     _cornell = undefined
>     _norway  = undefined
