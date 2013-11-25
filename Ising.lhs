% Haskell, Ising, Markov & Metropolis
% Dominic Steinitz
% 18th November 2013

---
bibliography: Ising.bib
---

Introduction
============

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

This is good for ferromagnetism: http://en.wikipedia.org/wiki/Ferromagnetism and this contains Curie temperatures: http://hyperphysics.phy-astr.gsu.edu/hbase/solids/ferro.html

Quotes from http://arxiv.org/pdf/0905.1629.pdf:

the drosophila of statistical mechanics

embarrassingly parallel


Acknowledgements
================

-- James Cook's package and comments

Haskell Preamble
================

Pragmas and imports to which only the over-enthusiastic reader need pay attention.

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}
>
> {-# LANGUAGE TypeFamilies                  #-}
>
> module Ising (
>        McState (..)
>        , main -- FIXME: For now just to get rid of warnings
>        ) where
>
> import qualified Data.Vector.Unboxed as V
> import qualified Data.Vector.Unboxed.Mutable as M
> import Data.Random.Source.PureMT
> import Data.Random
> import Control.Monad.State
>
> import Data.List.Split ( chunksOf )
> import Diagrams.Prelude hiding ( sample, render )
> import qualified Diagrams.Prelude as D
> import Diagrams.Coordinates ( (&) )
> import Diagrams.Backend.Cairo.CmdLine
>
> import Graphics.Rendering.Chart
> import Data.Default.Class
> import Graphics.Rendering.Chart.Backend.Cairo hiding (runBackend, defaultEnv)
> import Control.Lens hiding ( (#), (&) )

Other
=====

An explanation of the Boltzmann distribution (which we need to replay):

http://www.math.fsu.edu/~quine/MB_11/10%20HP%20model%20and%20Boltzmann%20distribution.pdf

An explanation of the Ising model using the Boltzmann distribution: http://www.uio.no/studier/emner/matnat/fys/FYS3150/h07/undervisningsmateriale/Lecture%20Notes/lecture2007.pdf

Monte Carlo Estimation
======================

Although Ising himself developed an analytic solution in 1 dimension
and Onsager later developed an analytic solution in 2 dimensions,
no-one has (yet) found an analytic solution for 3 dimensions.


One way to determine an approximate answer is to use a Monte Carlo
method.

Uniform Sampling
----------------


 We could pick sample configurations at random according to the
Boltzmann distribution

$$
\pi(\sigma) = \frac{\exp(-E(\sigma) / k_B T)}{Z(T)}
$$

where the sum $T$ is the temperature, $k_B$ is Boltzmann's constant,
$E$ is the energy of a given state

$$
E(\sigma) = -J\sum_{i, j} \sigma_i \sigma_j - B \sum_k \sigma_k
$$

and $Z(T)$ is a normalizing constant (Z for the German word
Zustandssumme, "sum over states")

$$
Z(T) = \sum_\sigma \exp(-E(\sigma) / k_B T)
$$

The standard notation for $k_B T$ is $\beta$.

We can evaluate the energy for one state easily enough.

As an aside, we represent each state by a *Vector* of *Int*. No doubt
more efficient representations can be implemented. We also have to
calculate offsets into the vector given a point's grid co-ordinates.

> energy :: V.Vector Int => Double
> energy v = 0.5 * (fromIntegral $ V.sum energyAux)
>   where
>
>     energyAux = V.generate l f
>
>     l = V.length v
>
>     f m = c * d
>       where
>         i = m `mod` gridSize
>         j = (m `mod` (gridSize * gridSize)) `div` gridSize
>
>         c = v V.! jc
>         jc = gridSize * i + j
>
>         d = n + e + s + w
>
>         n = v V.! jn
>         e = v V.! je
>         s = v V.! js
>         w = v V.! jw
>
>         jn = gridSize * ((i + 1) `mod` gridSize) + j
>         js = gridSize * ((i - 1) `mod` gridSize) + j
>         je = gridSize * i + ((j + 1) `mod` gridSize)
>         jw = gridSize * i + ((j - 1) `mod` gridSize)

But what about the normalizing constant $Z$? Even for a modest grid
size say $10 \times 10$, the number of states that needs to be summed
over is extremely large $2^{10 \times 10}$.

Instead of summing the entire state space, we could draw R random
samples $(\sigma^{(i)})_{0 \le i \lt R}$ uniformly from the state
space. We could then use

$$
Z_R = \sum_0^{R-1} P^*(\sigma^{(i)})
$$

to estimate e.g. the magnetization

$$
\langle M \rangle = \sum_\sigma M(\sigma) \frac{\exp(-\beta E(\sigma))}{Z(T)} \mathrm{d} \sigma
$$

by

$$
\widehat{\langle M \rangle} = \sum_{i=0}^{R-1} M(\sigma) \frac{exp(-\beta E(\sigma(i)))}{Z(T)}
$$


However we know from statistical physics that systems with large
numbers of particles will occupy a small portion of the state space
with any significant probability.  And according to [@MacKay:itp], a
high dimensional distribution is often concentrated on small region of
the state space known as its typical set $T$ whose volume is given by
$|T| \approx 2^H$ where $H$ is the entropy of the (Boltzmann)
distribution which for ease of exposition we temporarily denote by
$P$.

$$
H = -\sum_\sigma P(\sigma)\log_2(P(\sigma))
$$

If almost all the probability mass is located in $T$ then the actual
value of the (mean) magnetization will determined by the values that
$M$ takes on that set. So uniform sampling will only give a good
estimate if we make $R$ large enough that we hit $T$ at least a small
number of times. The total size of the state space is $2^N$ and the
$|T| \approx 2^H$, so there is a probability of $2^H / 2^N$ of hitting
$T$. Thus we need roughly $2^{N - H}$ samples to hit $T$.

At high temperatures, the Boltzmann distribution flattens out so
roughly all of the states have an equal likelihood of being
occupied. We can calculate the (Shannon) entropy for this.

$$
H \approx \sum_\sigma \frac{1}{2^N}\log_2 2^N = N
$$

We can do a bit better than this. At high temperatures, $\beta J \ll
1$ and taking $B = 0$

$$
\begin{aligned}
Z &= \sum_\sigma \exp\big(-\beta E(\sigma)\big) \\
  &= \sum_\sigma \exp\bigg(\beta J \sum_{i, j} \sigma_i \sigma_j - B \sum_k \sigma_k\bigg) \\
  &= \sum_\sigma \prod_{i, j} \exp\beta J \sigma_i \sigma_j
\end{aligned}
$$

By defintion

$$
\begin{aligned}
\cosh x &= \frac{e^x + e^{-x}}{2} \\
\sinh x &= \frac{e^x - e^{-x}}{2}
\end{aligned}
$$

Thus

$$
\begin{aligned}
\exp \beta J \sigma_i \sigma_j &= \cosh\beta J + \sigma_i\sigma_j\sinh \beta J \\
                               &= \cosh\beta J (1 + \sigma_i\sigma_j\tanh \beta J)
\end{aligned}
$$

We can therefore re-write the partition function as

$$
\begin{aligned}
Z &= \sum_\sigma \prod_{i, j} \cosh\beta J (1 + \sigma_i\sigma_j\tanh \beta J) \\
  &= (\cosh \beta J)^{4N/2} \sum_\sigma \prod_{i, j} (1 + \sigma_i\sigma_j\tanh \beta J) \\
\end{aligned}
$$

where the factor 2 is because we count the exchange interaction twice
for each pair of atoms and the factor 4 is because each atom is
assumed to only interact with 4 neighbours.

Since at high temperatures, by assumption, we have $\beta J \ll 1$
then $\tanh \beta J \ll 1$ also.

Thus

$$
Z \approx (\cosh \beta J)^{2N}\sum_\sigma 1 = 2^N(\cosh \beta J)^{2N}
$$

Calculating the free energy

$$
\begin{aligned}
F &= -k_B T \ln Z \\
  &= -k_B T N \ln 2 -k_B T 2 N \ln (\cosh \beta J) \\
  &\approx -k_B T N \ln 2 -k_B T N (\beta J)^2 \\
  &= -k_B T N \ln 2 - N\frac{J^2}{k_B T} \\
\end{aligned}
$$

From this we can determine the (Boltzmann) entropy

$$
S = k_B N (\ln 2 - (\beta J)^2) \approx k_B N \ln 2
$$

which agrees with our rather hand-wavy derivation of the (Shannon)
entropy at high temperatures.

At low temperatures the story is quite different.
We can calculate the ground state energy where all
the spins are in the same direction.

$$
E_0 = -J\sum_{i,j}\sigma_i\sigma_j = -4JN / 2 = -2JN
$$

And we can assume that at low temperatures that flipped spins are
isolated from other flipped spins. The energy for an atom in the
ground state is -4J and thus the energy change if it flips is 8J. Thus
the energy at low temperatures is

$$
E_1 \approx E_0 + 8J\sum_i (\sigma_i + 1) / 2
$$

The partition function is

$$
\begin{aligned}
Z &= 2\sum_\sigma \exp{-\beta \bigg(E_0 + 8J\sum_i (\sigma_i + 1) /2\bigg)} \\
  &= 2\exp{-\beta E_0} \prod_i \sum_{\sigma_i = \pm 1} \exp{-8\beta J(\sigma_i + 1) / 2} \\
  &= 2\exp{-\beta E_0} (1 + \exp{-8\beta J})^N
\end{aligned}
$$

Again we can calculate the free energy and ince we are doing this
calculation to get a rough estimate of the entropy we can approximate
further.

$$
\begin{aligned}
F &= -k_B T \ln Z \\
  &= -k_B (T \ln 2 - T E_0 + T N \ln (1 + \exp{-8\beta J})) \\
  &\approx -k_B (T \ln 2 - T E_0 + T N \exp{-8\beta J})
\end{aligned}
$$

From this we can determine the (Boltzmann) entropy. Using the fact that $\beta = 1 / k_B T$ and thus that

$$
\frac{\partial \beta}{\partial T} = - \frac{1}{k_B T^2} = -k_B\beta^2
$$

we have

$$
\begin{aligned}
S &= - \frac{\partial F}{\partial T} \\
  &= k_B\bigg(\ln 2 + N\frac{\partial}{\partial T} \exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial}{\partial T} \frac{1}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial \beta}{\partial T} \frac{\partial}{\partial \beta}\frac{1}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial \beta}{\partial T} \frac{\partial}{\partial \beta}\frac{1}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial \beta}{\partial T}
     \bigg(-\frac{1}{\beta^2}\exp{-8\beta J} -
     \frac{8J}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 +
     N\exp{-8\beta J} +
     8JN\beta\exp{-8\beta J}\bigg)
\end{aligned}
$$

The critical temperature (as we shall obtain by simulation) given by
[Onsager's exact
result](http://en.wikipedia.org/wiki/Ising_model#Onsager.27s_exact_solution)
in 2d is

$$
\frac{1}{\beta} = k_BT_{\text Crit} = \frac{2J}{\ln(\sqrt{2}+1)} \approx 2.269J
$$

Plugging this in we get

    [ghci]
    exp (-8.0/2.269) * (1.0 + 8.0 / 2.269)

Thus the (Shannon) entropy is about 0.13 at the interesting temperature.

Thus uniform sampling will provide reasonable estimates.


As usual we work on a measure space $(\Omega, {\mathbb F}, \mu)$.


http://streaming.stat.iastate.edu/~stat444x_B/Literature/ChibGreenberg.pdf

Let $S$ be a finite set. In the case of an Ising model with $N$ cells,
this set will contain $2^N$ elements. Let $P = \{ p_{ij} : i, j \in S
\}$ be such that

$$
\sum_{j \in S} p_{ij} = 1 \, \forall i \in S
$$


Markov Chains
=============

Metropolis and his team [@Metropolis53] discovered a way of
constructing a Markov chain with a limiting distribution of the
distribution required.

Markov first studied the stochastic processes that came to be named after him in 1906.

We follow [@DBLP:books/daglib/0095301], [@Beichl615768] and [@Gravner:mat135a:Online].


Stationarity
------------

A Markov chain has a **stationary distribution** $\pi_i$ if

$$
\sum_{i \in S} \pi_i p_{ji} = \pi_j
$$

One question one might ask is whether a given Markov chain has such a
distribution. For example, for the following chain, any distribution
is a stationary distribution. That is $\pi P = \pi$ for any $\pi$.

$$
\begin{bmatrix}
  1 & 0 \\
  0 & 1
 \end{bmatrix}
$$

Another key question is, if there is a unique stationary distribution,
will the $n$-th transion probabilities converge to that distribution
(FIXME: really badly expressed but will do as a reminder).

In the case of chains with a countably infinite state space, a

Irreducibility
--------------

Write ${\mathbb P}_i(A) = {\mathbb P}(A \, | \, X_0 = i)$

We say that $i$ **leads to** $j$ and write $i \rightarrow j$ if

$$
{\mathbb P}_i(X_n = j \, \text{for some n}) \gt 0
$$

**Theorem** For distinct states $i$ and $j$, the following are equivalent:

* $i \rightarrow j$
* $p_{i_0i_1} p_{i_1i_2} \ldots p_{i_{n-1}i_n} \gt 0$
* $p_{ij}^{(n)} \gt 0$

$\blacksquare$

This makes it clear that $\rightarrow$ is transitive and reflexive
hence an equivalence relation. We can therefore partition the states
into classes. If there is only one class then the chain is called
**irreducible**.

For example,

$$
\begin{bmatrix}
  \frac{1}{2} & \frac{1}{2} &           0 &           0 \\
  \frac{1}{2} & \frac{1}{2} &           0 &           0 \\
            0 &           0 & \frac{1}{4} & \frac{3}{4} \\
            0 &           0 &           0 &           1
 \end{bmatrix}
$$

has classes $\{1, 2\}$, $\{3\}$ and $\{4\}$ so is *not* irreducible.

On the other hand

$$
\begin{bmatrix}
  \frac{1}{2} & \frac{1}{2} &           0 \\
  \frac{1}{2} & \frac{1}{4} & \frac{1}{4} \\
            0 & \frac{1}{3} & \frac{2}{3}
\end{bmatrix}
$$

is irreducible.

Recurrence
----------

Let $X_n$ be a Markov chain. A state $i$ is **recurrent** if

$$
{\mathbb P} (X_n = i \, \text{i.o.}) = 1
$$

The **first passage time** is defined as

$$
T_j = \inf_{n \ge 1} \{X_n = j\}
$$

Note that the $\inf$ is taken over $n$ *strictly* greater than
1. Incidentally the first passage time is a stopping time but that any
discussion of that would take this already long article even longer.

The expecation of the $i$-th first passage time starting from $i$ is
denoted ${\mathbb E}_i(T_i)$.

Define $m_i = {\mathbb E}_i(T_i)$

**Theorem** Let $P$ be irreducible then the following are equivalent:

* Every state is positive recurrent.

* Some state is positive recurrent.

* P has an invariant distribution $\pi$ and in this case $\pi_i = 1 / m_i$.

$\blacksquare$

A state $i$ is **aperiodic** if $p_{nn} \gt 0$ for *all* sufficiently large $n$.

Example:

$$
\begin{bmatrix}
  0 & 1 \\
  1 & 0
 \end{bmatrix}
$$

FIXME: Put Haskell example here with ghci for the first few terms

**Theorem** Let $P$ be irreducible and aperiodic and suppose that $P$ has an
invariant distribution $\pi$. Let $\pi_0$ be any distribution (on the state space???). Suppose that $(X_n)_{n \ge 0}$ is Markov $(\pi_0, P)$ then

* $\mu(X_n = j) \rightarrow \pi_j$ as $n \rightarrow \infty$ for all $j$

$\blacksquare$

A proof of this theorem uses *coupling* developed by Doeblin; see
[@Gravner:mat135a:Online] for more details.

**Corollary** With the conditions of the preceeding Theorem

* $p_{ij}^{(n)} \rightarrow \pi_j$ as $n \rightarrow \infty$ for all $i, j$

$\blacksquare$

If the state space is infinite, the existence of a stationary
distribution is not guaranteed even if the Markov chain is
irreducible, see [@Srikant:ece534:Online] for more details.

The Ergodic Theorem
-------------------

An irreducible, aperiodic, positive recurrent Markov chain has a
unique stationary distribution, which is also the limiting
distribution

Such Markov chains are called ergodic

Define the number of visits to $i$ strictly before time $n$ as

$$
V_i(n) \triangleq \sum_{k = 0}^{n - 1}{\mathcal I}_{\{X_k = i\}}
$$

$V_i(n) / n$ is the proportion of time before $n$ spent in state $i$.

**Theorem** Let $(X_n)_{(n \ge 0)}$ be an irreducible Markov chain then

$$
{\mathbb P} \Bigg(\frac{V_i(n)}{n} \rightarrow \frac{1}{m_i} \, {\text as} \, n \rightarrow \infty \Bigg) = 1
$$

If further the chain is positive recurrent then for any bounded
function $f : {\mathbb I} \rightarrow {\mathbb R}$ then

$$

$$


\blacksquare

Other Other
-----------

A warm start but we could try a cold start with all spins up.


Calculate magnetization:

Calculate energy:

> data McState = McState { mcMagnetization :: !Double
>                        , mcMAvg          :: !Double
>                        , mcCount         :: !Int
>                        , mcNumSamples    :: !Int
>                        , mcGrid          :: !(V.Vector Int)
>                        }
>   deriving Show
>
> gridSize :: Int
> gridSize = 10
>
> measure :: Int
> measure = 100
>
> nitt :: Int
> nitt = 1000000
>
> tCrit :: Double
> tCrit = 2.0 / log (1.0 + sqrt 2.0) - 0.1
>
> magnetization :: (V.Unbox a, Num a) => V.Vector a => a
> magnetization = V.sum
>
>
> expDv :: Double -> V.Vector Double
> expDv t = V.generate 9 f
>   where
>     f n | odd n = 0.0
>     f n         = exp (((fromIntegral (8 - n)) - 4.0) * 2.0 / t)
>
> singleUpdate :: Int -> V.Vector Double -> McState -> (Int, Int, Double) -> McState
> singleUpdate measure expDvT u (i, j, r) = -- D.trace (show $ mcMAvg u) $
>   McState { mcMagnetization = newMag
>           , mcMAvg =
>             if (mcCount u) `mod` measure == 0
>             then mcMAvg u + newMag
>             else mcMAvg u
>           , mcCount = mcCount u + 1
>           , mcNumSamples =
>             if (mcCount u) `mod` measure == 0
>             then mcNumSamples u + 1
>             else mcNumSamples u
>           , mcGrid = newGrid
>           }
>   where
>     newGrid = if p > r
>               then V.modify (\v -> M.write v jc (-c)) v
>               else v
>
>     oldMag = mcMagnetization u
>
>     newMag = if p > r
>               then oldMag - 2 * (fromIntegral c)
>               else oldMag
>
>     v = mcGrid u
>
>     p = expDvT V.! (4 + c * d)
>
>     c = v V.! jc
>     jc = gridSize * i + j
>
>     d = n + e + s + w
>
>     n = v V.! jn
>     e = v V.! je
>     s = v V.! js
>     w = v V.! jw
>
>     jn = gridSize * ((i + 1) `mod` gridSize) + j
>     js = gridSize * ((i - 1) `mod` gridSize) + j
>     je = gridSize * i + ((j + 1) `mod` gridSize)
>     jw = gridSize * i + ((j - 1) `mod` gridSize)
>
> testData :: Int -> V.Vector (Int, Int, Double)
> testData m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 2)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)            1.0)
>            return (r, c, v)
>
> trial :: McState -> Double -> V.Vector (Int, Int, Double) -> McState
> trial s t = V.foldl (singleUpdate 1 (expDv t)) s
>
> trialInitState :: McState
> trialInitState = McState { mcMagnetization = fromIntegral $
>                                              magnetization trialGrid
>                          , mcMAvg = 0.0
>                          , mcCount = 0
>                          , mcNumSamples = 0
>                          , mcGrid = trialGrid
>                         }
>
> trialGrid :: V.Vector Int
> trialGrid = V.fromList $ concat $ initGridL
>   where
>     initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
>                 , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
>                 , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
>                 , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
>                 , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
>                 , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
>                 , [ 1,  1,  1, -1, -1,  1, -1, -1,  1, -1]
>                 , [ 1, -1, -1, -1,  1,  1, -1, -1,  1, -1]
>                 , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
>                 , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
>                 ]
>
> xs :: [Double]
> xs = getTemps 4.0 0.5 100
>
> getTemps :: Double -> Double -> Int -> [Double]
> getTemps h l n = [ m * x + c |
>                    w <- [1..n],
>                    let x = fromIntegral w ]
>   where
>     m = (h - l) / (fromIntegral n - 1)
>     c = l - m
>
> newGrids :: [McState]
> newGrids = map (\t -> trial trialInitState t (testData nitt)) xs
>
> main :: IO ()
> main = do print "Magnetization"
>           mapM_ putStrLn $
>             zipWith (\t x -> show t ++ " " ++
>                              show (mcMAvg x / fromIntegral nitt)) xs newGrids
>
>           renderableToPNGFile errChart 500 500 "Magnetism.png"
>           defaultMain $
>             (chessBoard (mcGrid $ newGrids!!0) # D.translate (0&0)) <>
>             (chessBoard (mcGrid $ newGrids!!1) # D.translate (12&0))
>
> boardSq :: (Transformable b, HasStyle b, TrailLike b, V b ~ R2) =>
>            Colour Double -> b
> boardSq c = square 1 # lw 0 # fc c
>
> chessBoard :: (Monoid c, Semigroup c, Transformable c, HasStyle c,
>                Juxtaposable c, HasOrigin c, TrailLike c, V c ~ R2) =>
>               V.Vector Int -> c
> chessBoard v
>   = vcat $ map hcat $ map (map boardSq)
>   $ chunksOf gridSize $ map f $ V.toList v
>   where
>     f (-1) = red
>     f   1  = blue
>     f _    = error "Unexpected spin"
>
> errChart :: Graphics.Rendering.Chart.Renderable ()
> errChart = toRenderable layout
>   where
>     sinusoid1 = plot_lines_values .~ [[ (x, abs $ mcMAvg $
>                                             trial trialInitState x (testData nitt))
>                                       | x <- xs]]
>               $ plot_lines_style  . line_color .~ opaque blue
>               $ plot_lines_title .~ "error"
>               $ def
>
>     layout = layout1_title .~ "Floating Point Error"
>            $ layout1_plots .~ [Left (toPlot sinusoid1)]
>            $ layout1_left_axis .~ errorAxis
>            $ layout1_bottom_axis .~ stepSizeAxis
>            $ def
>
>     errorAxis = laxis_title .~ "Minus log to base 2 of the error"
>               $ def
>
>     stepSizeAxis = laxis_title .~ "Minus log to base 2 of the step size"
>                  $ def
>
>
> testData' :: Int -> V.Vector (Int, Int, Double)
> testData' m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 1)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)           1.0)
>            return (r, c, v)
>

Bibliography and Resources
--------------------------
