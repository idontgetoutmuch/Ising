% Haskell, Ising, Markov & Metropolis
% Dominic Steinitz
% 18th November 2013

---
bibliography: Ising.bib
---

```{.dia height='300'}
import Diagrams
import Ising
dia = hcat [ isingGrid 20 (mcGrid $ multiUpdate initState 3.0 (randomUpdates 100))
           , strutX 2.0
           , isingGrid 20 (mcGrid $ multiUpdate initState 3.0 (randomUpdates 1000))
           , strutX 2.0
           , isingGrid 20 (mcGrid $ multiUpdate initState 3.0 (randomUpdates 10000))
           ]

```

Introduction
============

About a year ago there was a reddit post on the [Ising Model in
Haskell](http://www.reddit.com/r/haskell/comments/16uc2x/ising_model_in_haskell). The
discussion seems to have fizzled out but Ising models looked like a
perfect fit for Haskell using
[repa](http://hackage.haskell.org/package/repa). In the end it turns
out that they are *not* a good fit for repa, at least not using the
original formulation. It may turn out that we can do better with
[Swendson-Yang](http://en.wikipedia.org/wiki/Swendsen–Wang_algorithm)
or [Wolff](http://en.wikipedia.org/wiki/Wolff_algorithm). But that
belongs to another blog post.

We can get some parallelism at a gross level using the Haskell
[parallel](http://hackage.haskell.org/package/parallel) package via a
one line change to the sequential code. However, this does not really
show off Haskell's strengths in this area. In any event, it makes a
good example for "embarassingly simple" parallelism in Haskell, the
[vector](http://hackage.haskell.org/package/vector) package and random
number generation using the
[random-fu](http://hackage.haskell.org/package/random-fu) package.

The [Ising model](http://en.wikipedia.org/wiki/Ising_model) was (by
[Stigler's
law](http://en.wikipedia.org/wiki/Stigler%27s_law_of_eponymy))
proposed by Lenz in 1920 as a model for ferromagnetism, that is, the
magnetism exhibited by bar magnets. The phenomenon ferromagnetism is
so named because it was first observed in iron (Latin ferrum and
chemical symbol Fe). It is also exhibited, for example, by rare earths
such as gadolinium. Ferromagnetic materials lose their magnetism at a
critical temperature: the [Curie
temperature](http://en.wikipedia.org/wiki/Curie_temperature) (named
after Pierre not his more famous wife). This is an example of a phase
transition (ice melting into water is a more familiar example).

The Ising model (at least in 2 dimensions) predicts this phase
transition and can also be used to describe phase transitions in
alloys.

Abstracting the Ising model from its physical origins, one can think
of it rather like [Conway's Game of
Life](http://idontgetoutmuch.wordpress.com/2013/02/23/comonads-life-and-klein-bottles):
there is a grid and each cell on the grid is updated depending on the
state of its neighbours. The difference with the Game of Life is that
the updates are not deterministic but are random with the randomness
selecting which cell gets updated as well as whether it gets
updated. Thus we cannot update all the cells in parallel as would
happen if we used repa. The reader only interested in this abstraction
can go straight to the implementation (after finishing this
introduction).

The diagram below shows a 2 dimensional grid of cells. Each cell can
either be in an (spin) up state or (spin) down state as indicated by
the arrows and corresponding colours. The Ising model then applies a
parameterized set of rules by which the grid is updated. For certain
parameters the cells remain in a random configuration, that is the net
spin (taking up = 1 and down = -1) remains near zero; for other
parameters, the spins in the cells line up (not entirely as there is
always some randomness). It is this lining up that gives rise to
ferromagnetism.

```{.dia height='200'}
import Diagrams
import Ising
dia = isingGrid 8 (exampleGrid 8)
```

On the other hand, the physics and the Monte Carlo method used to
simulate the model are of considerable interest in their own
right. Readers interested in the Monte Carlo method can skip the
physics and go to Monte Carlo Estimation. Readers interested in the
physics can start with the section on Magnetism.

Definitions, theorems etc. are in **bold** and terminated by $\blacksquare$.

Magnetism
=========

Following Ziman [@Ziman:Principles] and Reif [@reif2009fundamentals],
we assume that each atom in the ferromagnetic material behaves like a
small magnet. According to [Hund's
rules](http://hyperphysics.phy-astr.gsu.edu/hbase/atomic/hund.html),
we would expect unpaired electrons in the $d$ and $f$ shells for
example in the transition elements and rare earths and these would
supply the magnetic moment. However, the magnetic interaction between
atoms is far too small to account for ferromagnetism. For Iron, for
example, the Curie temperature is 1043K and the magnetic interaction
between atoms cannot account for this by some margin. There is a
quantum mechanical effect called the exchange interaction which is a
consequence of the Pauli exclusion principle. Two electrons with the
same spin on neighbouring atoms cannot get too close to each other. If
the electrons have anti-parallel spins then the exclusion principle
does not apply and there is no restriction on how close they can get
to each other. Thus the electrostatic interaction between electrons on
neighbouring atoms depends on whether spins are parallel or
anti-parallel. We can thus write the Hamiltonian in the form:

$$
E = -J\sum_{\langle i, j\rangle} \sigma_i \sigma_j - B\sum_k \sigma_k
$$

Where

 * The notation $\langle i, j\rangle$ means we sum over all the
nearest neighbours in the lattice;

 * $B$ is the applied magnetic field (note we use E for the
Hamiltonian), for all of this article we will assume this to be $0$;

 * The range of each index is $1 \ldots M$ where $N = M \times M$ is
the total number of atoms;

 * And $J$ the **coupling constant** expressing the strength of the
interaction between neighboring spins and depending on the balance
between the Pauli exclusion principle and the electrostatic
interaction energy of the electrons, this may be positive
corresponding to parallel spins (ferromagnetism which is the case we
consider in this article) or negative corresponding to anti parallel
spins (antiferromagnetism or ferrimagnetism which we consider no
further).

Acknowledgments
================

This post uses the
[random-fu](http://hackage.haskell.org/package/random-fu) package for
random number generation and has also benefitted from comments by the
author of that package (James Cook).

All diagrams were drawn using the Haskell
[diagrams](http://projects.haskell.org/diagrams) domain specific
language; the inhabitants of #diagrams were extremely helpful in
helping create these.

Internet sources too numerous to mention were used for the physics and
Monte Carlo. Some are listed in the bibliography. Apologies if you
recognize something which does not get its just acknowledgement. The
advantage of blog posts is that this can easily be remedied by leaving
a comment.

Haskell Preamble
================

Pragmas and imports to which only the over-enthusiastic reader need pay attention.

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}

> {-# LANGUAGE TypeFamilies                  #-}
> {-# LANGUAGE NoMonomorphismRestriction     #-}
> {-# LANGUAGE TypeOperators                 #-}
> {-# LANGUAGE ScopedTypeVariables           #-}

> module Ising (
>          energy
>        , magnetization
>        , McState(..)
>        , thinN
>        , gridSize
>        , nItt
>        , expDv
>        , tCrit
>        , singleUpdate
>        , randomUpdates
>        , initState
>        , multiUpdate
>        , temps
>        , initColdGrid
>        , exampleGrid
>        , notAperiodics
>        , main
>        ) where

We put all of our code for the diagrams in this blog post in a
separate module to avoid clutter.

> import Diagrams

> import qualified Data.Vector.Unboxed as V
> import qualified Data.Vector.Unboxed.Mutable as M

> import Data.Random.Source.PureMT
> import Data.Random
> import Control.Monad.State

> import Control.Parallel.Strategies

> import Graphics.Rendering.Chart.Backend.Cairo

> import Data.Array.Repa hiding ( map, (++), zipWith )
> import Data.Array.Repa.Algorithms.Matrix

> import PrettyPrint ()

> gridSize :: Int
> gridSize = 20
>
> exampleGrid :: Int -> V.Vector Int
> exampleGrid gridSize = V.fromList $ f (gridSize * gridSize)
>   where
>     f m =
>       evalState (replicateM m (sample (uniform (0 :: Int) (1 :: Int)) >>=
>                                \n -> return $ 2*n - 1))
>       (pureMT 1)

> couplingConstant :: Double
> couplingConstant = 1.0
>
> kB :: Double
> kB = 1.0
>
> mu :: Double
> mu = 1.0

The Boltzmann Distribution
==========================

Statistical physics is an extremely large subject but there needs to
be some justification for the use of the Boltzmann distribution. For
an excellent introduction to the subject of Statistical Physics (in
which the Boltzmann distribution plays a pivotal role) see David
Tong's lecture notes (@Tong:Statphys:Online).

Suppose we have we 3 boxes (we use the more modern nomenclature rather
than the somewhat antiquated word urn) and 7 balls and we randomly
assign the balls to boxes. Then it is far more likely that we will get
an assignment of 2,2,3 rather than 0,0,7. When the numbers of boxes
and balls become large (which is the case in statistical physics where
we consider e.g. $10^{23}$ numbers of atoms) then it becomes very, very,
very likely that the balls (or atoms) will spread themselves out
uniformly as in the example.

Now suppose we have $N$ balls and $M$ boxes and we associate an energy
$k$ with the $k$-th box but restrict the total energy to be
constant. Thus we have two constraints:

* The total number of balls $N = \sum_1^M n_k$ and

* The total energy $E = \sum_1^M k\, n_k$.

Let us assume the balls are allocated to boxes in the most likely way
and let us denote the values in each box for this distribution as
$\tilde{n}_k$.

Let us now move 2 balls from box $k$, one to box $k-1$ and one to box
$k+1$. Note that both constraints are satisified by this move. We must
therefore have

$$
\begin{aligned}
\frac{1}{(\tilde{n}_{k-1} + 1)!}\frac{1}{(\tilde{n}_{k} - 2)!}\frac{1}{(\tilde{n}_{k+1} + 1)!} &\le \frac{1}{\tilde{n}_{k-1}!}\frac{1}{\tilde{n}_{k}!}\frac{1}{\tilde{n}_{k+1}!} \\
\frac{\tilde{n}_k}{\tilde{n}_{k-1} + 1}\frac{\tilde{n}_k - 1}{\tilde{n}_{k+1} + 1} &\le 1
\end{aligned}
$$

And let us start from the most likely distribution and move 1 ball
from box $k-1$ and 1 ball from box $k+1$ into box $k$. Again both
constraints are satisified by this move. Doing a similar calculation
to the one above we get

$$
\begin{aligned}
\frac{1}{(\tilde{n}_{k-1} - 1)!}\frac{1}{(\tilde{n}_{k} + 2)!}\frac{1}{(\tilde{n}_{k+1} - 1)!} &\le \frac{1}{\tilde{n}_{k-1}!}\frac{1}{\tilde{n}_{k}!}\frac{1}{\tilde{n}_{k+1}!} \\
1 &\le \frac{\tilde{n}_k + 1}{\tilde{n}_{k+1}}\frac{\tilde{n}_{k} + 2}{\tilde{n}_{k-1}}
\end{aligned}
$$

From this we deduce that as $n_k \rightarrow \infty$ then $n_k^2
\approx n_{k-1}n_{k+1}$ or that $n_{k+1} / n_k = B$ for some constant
$B$.

Telecsoping we can write $n_k = n_1 B^{k-1} \propto B^k$. Thus the
probability of a ball being in box $k$ is

$$
\mathbb{P}(k) = \frac{B^k}{\sum_{i=1}^M B^i}
$$

If we now set $B = e^{-\beta}$ and $Z = \sum_{i=1}^M B^i$ then we can
re-write our distribution as

$$
\mathbb{P}(k) = \frac{e^{-\beta k}}{Z}
$$

It should therefore be plausible that the probability of finding a
system with a given energy $E(\sigma)$ in a given state $\sigma$ is given by
the Boltzmann distribution

$$
\mathbb{P}(\sigma) = \frac{\exp(-E(\sigma) / k_B T)}{Z(T)}
$$

where we have defined the temperature to be $T = 1 / k_B\beta$ with
$k_B$ being Boltzmann's constant and $Z(T)$ is another normalizing
constant.

Specific Heat
-------------

Using the Boltzmann distribution we can calculate the average energy of the system

$$
\begin{aligned}
\langle E \rangle &\triangleq  \sum_\sigma E(\sigma) \mathbb{P}(\sigma) \\
                  &=  \sum_\sigma E(\sigma) \frac{\exp(-E(\sigma) / k_B T)}{Z} \\
                  &= -\frac{1}{Z} \frac{\partial Z}{\partial \beta} \\
                  &=  \frac{\partial}{\partial \beta} \log Z
\end{aligned}
$$

where it is understood that $Z$ depends on $T$.

If we let $C_V$ be the specific heat per particle (at constant volume) then

$$
\begin{aligned}
NC_V &\triangleq \frac{\partial \langle E\rangle}{\partial T} \\
     &= \frac{\partial \langle E\rangle}{\partial \beta} \frac{\partial \beta}{\partial T} \\
     &= -\frac{1}{k_B T^2} \frac{\partial \langle E\rangle}{\partial \beta} \\
     &= -\frac{1}{k_B T^2} \frac{\partial^2 \log Z}{\partial \beta^2} \\
     &= -\frac{1}{k_B T^2} \frac{\partial}{\partial \beta} \sum_\sigma E(\sigma) \frac{\exp(-E(\sigma) / k_B T)}{Z} \\
     &= -\frac{1}{k_B T^2} \Bigg[\frac{\big(\sum_\sigma E(\sigma) \exp(-E(\sigma) / k_B T)\big)^2}{Z^2} + \frac{\sum_\sigma -E^2(\sigma) \exp(-E(\sigma) / k_B T)}{Z}\Bigg]
\end{aligned}
$$

We know that

$$
\Delta E^2 \triangleq \langle (E - \langle E \rangle)^2 \rangle = \langle E^2 \rangle - \langle E \rangle^2
$$

So we can write

$$
\Delta E^2 = k_BT^2C_V
$$

so if we can estimate the energy and the square of the energy then we
can estimate the specific heat.

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
\mathbb{P}(\sigma) = \frac{\exp(-E(\sigma) / k_B T)}{Z(T)}
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

We can evaluate the energy for one state easily enough as the Haskell
below demonstrates. Note that we use so-called periodic boundary
conditions which means our grid is actually a torus with no
boundaries. In other words, we wrap the top of the grid on to the
bottom and the left of the grid on to the right.

[As an aside, we represent each state by a *Vector* of *Int*. No doubt
more efficient representations can be implemented. We also have to
calculate offsets into the vector given a point's grid co-ordinates.]

> energy :: (Fractional a, Integral b, M.Unbox b) => a -> V.Vector b -> a
> energy j v = -0.5 * j * (fromIntegral $ V.sum energyAux)
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
Z_R \triangleq \sum_0^{R-1} \exp (-\beta\sigma_i)
$$

to estimate e.g. the magnetization

$$
\langle M \rangle = \sum_\sigma M(\sigma) \frac{\exp(-\beta E(\sigma))}{Z(T)} \mathrm{d} \sigma
$$

by

$$
\widehat{\langle M \rangle} = \sum_{i=0}^{R-1} M(\sigma) \frac{exp(-\beta E(\sigma(i)))}{Z_R}
$$


However we know from statistical physics that systems with large
numbers of particles will occupy a small portion of the state space
with any significant probability.  And according to [@MacKay:itp
Chapter 29], a high dimensional distribution is often concentrated on
small region of the state space known as its typical set $T$ whose
volume is given by $|T| \approx 2^H$ where $H$ is the (Shannon)
entropy of the (Boltzmann) distribution which for ease of exposition
we temporarily denote by $P$.

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
1$ and taking $B = 0$ (as we have been assuming all along)

$$
\begin{aligned}
Z &= \sum_\sigma \exp\big(-\beta E(\sigma)\big) \\
  &= \sum_\sigma \exp\bigg(\beta J \sum_{i, j} \sigma_i \sigma_j - B \sum_k \sigma_k\bigg) \\
  &= \sum_\sigma \prod_{i, j} \exp\beta J \sigma_i \sigma_j
\end{aligned}
$$

By definition

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

Again we can calculate the free energy and since we are doing this
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
\frac{1}{\beta} = k_BT_{\text Crit} = \frac{2J}{\ln(\sqrt{2}+1)}
$$

Taking $J = 1$

> tCrit :: Double
> tCrit = 2.0 * j / log (1.0 + sqrt 2.0) where j = 1

    [ghci]
    tCrit

Plugging this in we get

    [ghci]
    exp (-8.0/tCrit) * (1.0 + 8.0 / tCrit)

Thus the (Shannon) entropy is about 0.13N at the interesting
temperature and is about N at high temperatures. So uniform sampling
would require $\sim 2^{(N - N)}$ samples at high temperatures but $\sim
2^{(N - 0.13N)} \approx 2^{N /2}$ at temperatures of interest. Even for
our modest $10 \times 10$ grid this is $2^{50} \approx 10^{17}$ samples!

Fortunately, Metropolis and his team [@Metropolis53] discovered a way
of constructing a Markov chain with a limiting distribution of the
distribution required which does not require the evaluation of the
partition function and which converges in a reasonable time (although
theoretical results substantiating this latter point seem to be hard
to come by).

Markov Chains
=============

Markov first studied the stochastic processes that came to be named after him in 1906.

We follow [@DBLP:books/daglib/0095301], [@Beichl615768],
[@Diaconis:1995:WKM:225058.225095], [@Greenbert95] [@citeulike:7907579] and
[@Gravner:mat135a:Online].

As usual we work on a probability measure space $(\Omega, {\mathbb F},
\mathbb{P})$ (that is $\mathbb{P}(\Omega) = 1$). Although this may not be much in
evidence, it is there lurking behind the scenes.

Let $S$ be a finite set. In the case of an Ising model with $N$ cells,
this set will contain $2^N$ elements (all possible configurations).

A **Markov chain** is a discrete time stochastic process $X_0, X_1,
\ldots$ such that

$$
\mathbb{P} (X_{n+1} = j \,|\, X_0 = i_0, X_1 = i_1, \dots X_n = i) = \mathbb{P} (X_{n+1} = j \,|\, X_n = i)
$$

$\blacksquare$

That is, where a Markov chain goes next only depends on where it is
not on its history.

A **stochastic transition matrix** is a matrix $P = \{ p_{ij} : i, j \in S
\}$ such that

$$
\sum_{j \in S} p_{ij} = 1 \, \forall i \in S
$$

$\blacksquare$

We can describe a Markov chain by its transition matrix $P$ and
initial distribution $\pi_0(i) = \mathbb{P} (X_0 = i)$. In the case we
say a stochastic process $(X_n)_{n \ge 0}$ is Markov $(\pi_0, P)$.

We need to be able to discuss properties of Markov chains such as
stationarity, irreducibility, recurrence and ergodicity.

Stationarity
------------

A Markov chain has a **stationary distribution** $\pi(i)$ if

$$
\sum_{i \in S} \pi(i) p_{ij} = \pi(j)
$$

$\blacksquare$

One question one might ask is whether a given Markov chain has such a
distribution. For example, for the following chain, any distribution
is a stationary distribution. That is $\pi P = \pi$ for any $\pi$.

Any distribution is a stationary distribution for the unit transition matrix.

$$
\begin{bmatrix}
  1 & 0 \\
  0 & 1
 \end{bmatrix}
$$

The $n$-th transition matrix of a Markov chain is $P^n$. The
corresponding matrix entries are

$$
p^{(n)}_{ij} = (P^n)_{ij}
$$

Another key question is, if there is a unique stationary distribution,
will the $n$-th transition probabilities converge to that
distribution, that is, when does, $p^{(n)}_{ij} \rightarrow \pi(j)$ as
$n \rightarrow \infty$.

Irreducibility
--------------

Write ${\mathbb P}_i(A) = {\mathbb P}(A \, | \, X_0 = i)$

We say that $i$ **leads to** $j$ and write $i \rightarrow j$ if

$$
{\mathbb P}_i(X_n = j \, \text{eventually}) \gt 0
$$

**Theorem** For distinct states $i$ and $j$, the following are equivalent:

* $i \rightarrow j$
* $p_{i_0i_1} p_{i_1i_2} \ldots p_{i_{n-1}i_n} \gt 0$
* $p_{ij}^{(n)} \gt 0$ for some $n \ge 0$

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

Let $(X_n)_{n \ge 0}$ be a Markov chain. A state $i$ is **recurrent** if

$$
{\mathbb P} (X_n = i \, \text{infinitely often}) = 1
$$

The **first passage time** is defined as

$$
T_j = \inf_{n \ge 1} \{X_n = j\}
$$

Note that the $\inf$ is taken over $n$ *strictly* greater than
1. Incidentally the first passage time is a stopping time but that any
discussion of that would take this already long article even longer.

The expectation of the $i$-th first passage time starting from $i$ is
denoted $m_i = {\mathbb E}_i(T_i)$.

**Theorem** Let $P$ be irreducible then the following are equivalent:

* Every state is positive recurrent.

* Some state is positive recurrent.

* P has an invariant distribution $\pi$ and in this case $\pi(i) = 1 / m_i$.

$\blacksquare$

A state $i$ is **aperiodic** if $p^{(n)}_{ii} \gt 0$ for *all*
sufficiently large $n$.

For example, the chain with this transition matrix is *not* periodic:

$$
\begin{bmatrix}
  0 & 1 \\
  1 & 0
 \end{bmatrix}
$$

as running the following program segment shows with the chain
flip-flopping between the two states.

> notAperiodic0 :: Array U DIM2 Double
> notAperiodic0 = fromListUnboxed (Z :. (2 :: Int) :. (2 :: Int)) ([0,1,1,0] :: [Double])
> notAperiodics :: [Array U DIM2 Double]
> notAperiodics = scanl mmultS notAperiodic0 (replicate 4 notAperiodic0)

    [ghci]
    import Ising
    import PrettyPrint
    import Text.PrettyPrint.HughesPJClass
    pPrint notAperiodics

**Theorem** Let $P$ be irreducible and aperiodic and suppose that $P$ has an
invariant distribution $\pi$. Let $\pi_0$ be any distribution (on the state space???). Suppose that $(X_n)_{n \ge 0}$ is Markov $(\pi_0, P)$ then

* $\mathbb{P}(X_n = j) \rightarrow \pi(j)$ as $n \rightarrow \infty$ for all $j$

$\blacksquare$

A proof of this theorem uses *coupling* developed by Doeblin; see
[@Gravner:mat135a:Online] for more details.

**Corollary** With the conditions of the preceding Theorem

* $p_{ij}^{(n)} \rightarrow \pi(j)$ as $n \rightarrow \infty$ for all $i, j$

$\blacksquare$

If the state space is infinite, the existence of a stationary
distribution is not guaranteed even if the Markov chain is
irreducible, see [@Srikant:ece534:Online] for more details.

Detailed Balance
----------------

A stochastic matrix $P$ and a distribution $\pi$ are said to be in
**detailed balance** if

$$ \pi(i) p_{ij} = \pi(j) p_{ji} $$

$\blacksquare$

**Theorem** If a stochastic matrix $P$ and a distribution $\pi$ are in detailed
balance then $\pi$ is a stationary distribution.

$\blacksquare$

The Ergodic Theorem
-------------------

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
{\mathbb P} \Bigg(\frac{1}{n} \sum_{k = 0}^{n - 1}f(X_i)\rightarrow \hat{f} \, {\text as} \, n \rightarrow \infty \Bigg) = 1
$$

If further still the chain is aperiodic then it has a unique
stationary distribution, which is also the limiting distribution

$\blacksquare$

A Markov chain satisfying all three conditions is called ergodic.

The Metropolis Algorithm
------------------------

Thus if we can find a Markov chain with the required stationary
distribution and we sample a function of this chain we will get an
estimate for the average value of the function. What Metropolis and
his colleagues did was to provide a method of producing such a chain.

**Algorithm** Let $\pi$ be a probability distribution on the state space
$\Omega$ with $\pi(i) \gt 0$ for all $i$ and let $(Q, \pi_0)$ be an
ergodic Markov chain on $\Omega$ with transition probabilities $q(i,j)
\gt 0$ (the latter condition is slightly stronger than it need be but
we will not need fully general conditions).

Create a new (ergodic) Markov chain with transition probabilities

$$
p_{ij} =
\begin{cases}
q(i,j)\bigg[\frac{\pi(j) q(j,i)}{\pi(i) q(i,j)} \land 1 \bigg] & \text{if } j \ne i \\
1 - \sum_{k : k \ne i} q(i,k) \bigg[\frac{\pi(j) q(j,i)}{\pi(i) q(i,j)} \land 1 \bigg] & \text{if } j = i
\end{cases}
$$

where $\land$ takes the maximum of its arguments.

Calculate the value of interest on the state space e.g. the total
magnetization for each step produced by this new chain.

Repeat a sufficiently large number of times and take the average. This
gives the estimate of the value of interest.

$\blacksquare$

Let us first note that the Markov chain produced by this algorithm
almost trivially satisfies the detailed balance condition, for
example,

$$
\begin{aligned}
\pi(i) q(i,j)\bigg[\frac{\pi(j) q(j, i)}{\pi(i)q(i,j)} \land 1\bigg]
&= \pi(i)q(i,j) \land \pi(j)q(j,i) \\
&= \pi(j)q(j,i)\bigg[\frac{\pi(i) q(i, j)}{\pi(j)q(j,i)} \land 1\bigg]
\end{aligned}
$$

Secondly since we have specified that $(Q, \pi_0)$ is ergodic then
clearly $(P, \pi_0)$ is also ergodic (all the transition probabilities
are $\gt 0$).

So we know the algorithm will converge to the unique distribution we
specified to provide estimates of values of interest.

Two techniques that seem to be widespread in practical applications
are *burn in* and *thinning*. Although neither have strong theoretical
justification ("a thousand lemmings can't be wrong"), we follow the
practices in our implementation.

* "Burn in" means run the chain for a certain number of iterations
before sampling to allow it to forget the initial distribution.

* "Thinning" means sampling the chain every $n$ iterations rather than
every iteration to prevent autocorrelation.

Haskell Implementation
----------------------

Calculating the total magnetization is trivial; we just add up all the
spins and multiply by the magnetic moment of the electron.

> magnetization :: (Num a, Integral b, M.Unbox b) => a -> V.Vector b -> a
> magnetization mu = (mu *) . fromIntegral . V.sum

We keep the state of the Monte Carlo simulation in a record.

> data McState = McState { mcMagnetization :: !Double
>                        , mcMAvg          :: !Double
>                        , mcEnergy        :: !Double
>                        , mcEAvg          :: !Double
>                        , mcEAvg2         :: !Double
>                        , mcCount         :: !Int
>                        , mcNumSamples    :: !Int
>                        , mcGrid          :: !(V.Vector Int)
>                        }
>   deriving Show

As discussed above we sample every *thinN* iterations, a technique known as "thinning".

> thinN :: Int
> thinN = 100

The total number of iterations per Monte Carlo run.

> nItt :: Int
> nItt = 1000000

There are only a very limited number of energy changes that can occur
for each spin flip. Rather the recalculate the value of the Boltzmann distribution for
every spin flip we can store these in a *Vector*.

For example if spin is up and *all* its surrounding spins are up and
it flips to down then energy change is $8J$ and the corresponding
value of the Boltzmann distribution is $\exp -8J\beta$.

```{.dia width='500'}
import Diagrams
dia = eFlipD [  0,  1,  0,  1,  1,  1,  0,  1, 0]
             [  0,  1,  0,  1, -1,  1,  0,  1, 0]

```

Another example, the energy before is $-2J$ and the energy after is
$2J$ so the energy change is $4J$.

```{.dia width='500'}
import Diagrams
dia = eFlipD [  0,  1,  0, -1,  1,  1,  0,  1, 0]
             [  0,  1,  0, -1, -1,  1,  0,  1, 0]

```

> expDv :: Double -> Double -> Double -> V.Vector Double
> expDv kB j t = V.generate 9 f
>   where
>     f n | odd n = 0.0
>     f n         = exp (j * ((fromIntegral (8 - n)) - 4.0) * 2.0 / (kB * t))

The most important function is the single step update of the Markov
chain. We take an *Int* representing the amount of thinning we wish to
perform, the vector of pre-calculated changes of the Boltzmann
distribution, the current state, a value representing the randomly
chosen co-ordinates of the grid element that will be updated and a
value sampled from the uniform distribution which will decide whether
the spin at the co-ordinates will be updated.

> singleUpdate :: Int -> V.Vector Double -> McState -> (Int, Int, Double) -> McState
> singleUpdate thinN expDvT u (i, j, r) =
>   McState { mcMagnetization = magNew
>           , mcMAvg          = mcMAvgNew
>           , mcEnergy        = enNew
>           , mcEAvg          = mcEAvgNew
>           , mcEAvg2         = mcEAvg2New
>           , mcCount         = mcCount u + 1
>           , mcNumSamples    = mcNumSamplesNew
>           , mcGrid          = gridNew
>           }
>   where
>
>     (gridNew, magNew, enNew) =
>       if p > r
>       then ( V.modify (\v -> M.write v jc (-c)) v
>            , magOld - fromIntegral (2 * c)
>            , enOld + couplingConstant * fromIntegral (2 * c * d)
>            )
>       else (v, magOld, enOld)
>
>     magOld = mcMagnetization u
>     enOld  = mcEnergy u
>
>     (mcMAvgNew, mcEAvgNew, mcEAvg2New, mcNumSamplesNew) =
>       if (mcCount u) `mod` thinN == 0
>       then ( mcMAvgOld       + magNew
>            , mcEAvgOld       + enNew
>            , mcEAvg2Old      + enNew2
>            , mcNumSamplesOld + 1
>            )
>       else (mcMAvgOld, mcEAvgOld, mcEAvg2Old, mcNumSamplesOld)
>
>     enNew2 = enNew * enNew
>
>     mcMAvgOld       = mcMAvg u
>     mcEAvgOld       = mcEAvg u
>     mcEAvg2Old      = mcEAvg2 u
>     mcNumSamplesOld = mcNumSamples u
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

In order to drive our Markov chain we need a supply of random
positions and samples from the uniform distribution.

> randomUpdates :: Int -> V.Vector (Int, Int, Double)
> randomUpdates m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 1)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)            1.0)
>            return (r, c, v)

To get things going, we need an initial state. We start with a cold
grid, that is, one with all spins pointing down.

> initColdGrid :: V.Vector Int
> initColdGrid = V.fromList $ replicate (gridSize * gridSize) (-1)
>
> initState :: McState
> initState = McState { mcMagnetization      = magnetization mu initColdGrid
>                          , mcMAvg          = 0.0
>                          , mcEnergy        = energy couplingConstant initColdGrid
>                          , mcEAvg          = 0.0
>                          , mcEAvg2         = 0.0
>                          , mcCount         = 0
>                          , mcNumSamples    = 0
>                          , mcGrid          = initColdGrid
>                         }

We will want to run the simulation over a range of temperatures in
order to determine where the phase transition occurs.

> temps :: [Double]
> temps = getTemps 4.0 0.5 100
>   where
>     getTemps :: Double -> Double -> Int -> [Double]
>     getTemps h l n = [ m * x + c |
>                        w <- [1..n],
>                        let x = fromIntegral w ]
>       where
>         m = (h - l) / (fromIntegral n - 1)
>         c = l - m

Now we can run a chain at a given temperature.

> multiUpdate :: McState -> Double -> V.Vector (Int, Int, Double) -> McState
> multiUpdate s t = V.foldl (singleUpdate thinN (expDv kB couplingConstant t)) s

For example running the model at a temperature of $3.0$ for 100, 1000
and 10,000 steps respectively shows disorder growing.

```{.dia height='300'}
import Diagrams
import Ising
dia = hcat [ isingGrid 20 (mcGrid $ multiUpdate initState 3.0 (randomUpdates 100))
           , strutX 2.0
           , isingGrid 20 (mcGrid $ multiUpdate initState 3.0 (randomUpdates 1000))
           , strutX 2.0
           , isingGrid 20 (mcGrid $ multiUpdate initState 3.0 (randomUpdates 10000))
           ]
```

On the other hand running the model at a temperature of $2.0$ shows a
very limited disorder.

```{.dia height='300'}
import Diagrams
import Ising
dia = hcat [ isingGrid 20 (mcGrid $ multiUpdate initState 2.0 (randomUpdates 100))
           , strutX 2.0
           , isingGrid 20 (mcGrid $ multiUpdate initState 2.0 (randomUpdates 1000))
           , strutX 2.0
           , isingGrid 20 (mcGrid $ multiUpdate initState 2.0 (randomUpdates 10000))
           ]
```

For any given state we can extract the magnetization, the energy and
the square of the energy.

> magFn :: McState -> Double
> magFn s = abs (mcMAvg s / (fromIntegral $ mcNumSamples s))
>
> magFnNorm :: McState -> Double
> magFnNorm s = magFn s / (fromIntegral (gridSize * gridSize))
>
> enFn :: McState -> Double
> enFn s  = mcEAvg s / (fromIntegral $ mcNumSamples s)
>
> enFnNorm :: McState -> Double
> enFnNorm s = enFn s / (fromIntegral (gridSize * gridSize))

> en2Fn :: McState -> Double
> en2Fn s  = mcEAvg2 s / (fromIntegral $ mcNumSamples s)

And we can also calculate the mean square error.

> meanSqErr :: McState -> Double
> meanSqErr s = e2 - e*e
>   where
>     e = enFn s
>     e2 = en2Fn s
>
> meanSqErrNorm :: McState -> Double
> meanSqErrNorm s = meanSqErr s / (fromIntegral (gridSize * gridSize))

Finally we can run our simulation in parallel using *parMap* rather
than *map* (this is the one line change required to get parallelism).

> main :: IO ()
> main = do print "Start"
>
>           let rs = parMap rpar f temps
>                     where
>                       f t = multiUpdate initState t (randomUpdates nItt)
>
>           renderableToFile (FileOptions (500, 500) PNG)
>                            (errChart temps rs magFnNorm enFnNorm meanSqErrNorm)
>                           "diagrams/MagnetismAndEnergy.png"
>
>           print "Done"


```{.dia width='500'}
dia = image "diagrams/MagnetismAndEnergy.png" 1.0 1.0
```

Performance and Parallelism
---------------------------

~~~~ { .shell }
ghc -O2 Ising.lhs -threaded -o Ising -package-db=.cabal-sandbox/x86_64-osx-ghc-7.6.2-packages.conf.d/ -main-is Ising
~~~~

~~~~ { .shell }
time ./Ising +RTS -N1
"Start"
"Done"

real	0m14.879s
user	0m14.508s
sys	0m0.369s

time ./Ising +RTS -N2
"Start"
"Done"

real	0m8.269s
user	0m15.521s
sys	0m0.389s

time ./Ising +RTS -N4
"Start"
"Done"

real	0m5.444s
user	0m19.386s
sys	0m0.414s
~~~~

Bibliography and Resources
--------------------------

The sources for this article can be downloaded
[here](https://github.com/idontgetoutmuch/Ising).

